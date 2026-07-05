open Lwt.Syntax
open Libvolley

(* Authoritative volley server with rollback, hosting many games at once.

   Each game is identified by a random 5-digit code. A client's first message
   picks a game: [Create] opens a fresh one (the client becomes player 1 and is
   told the code to share), [Join code] takes the remaining slot of an existing
   one. A full game answers [Full]; an unknown code answers [Unknown_game].

   The server owns each simulation and ticks it at a fixed 60fps. Instead of
   applying "whatever input arrived last" at each tick (which lands inputs at
   the wrong sim-time and feels jittery), it keeps a ~1 second window of every
   frame's state and inputs. Each client tags its input with the frame it was
   reacting to; when that input arrives (necessarily a little late) the server
   inserts it at that frame and replays the simulation forward to the present.
   This makes the simulation consistent regardless of network jitter, never
   drops a single-frame jump, and gives us round-trip time for free.

   A game only simulates while both players are present; with a single player
   it stays reset and sends [Waiting] each tick, and it is deleted once empty.

 *)

let port = 8080
let dt = 1.0 /. 60.0

(* Artificial latency for testing on one machine. [VOLLEY_RTT_MS] is the
   simulated round-trip in milliseconds; we apply half of it to inbound inputs
   and half to outbound state, so the measured ping matches it. Default 0. *)
let artificial_rtt =
  match Sys.getenv_opt "VOLLEY_RTT_MS" with
  | Some s -> ( try float_of_string s /. 1000.0 with _ -> 0.0)
  | None -> 0.0

let half_lag = artificial_rtt /. 2.0

(* Run [f ()] now, or after the one-way artificial delay, fire-and-forget. *)
let with_lag f =
  if half_lag > 0.0 then
    Lwt.async (fun () ->
        let* () = Lwt_unix.sleep half_lag in
        f ())
  else Lwt.async f

let window = 60 (* keep ~1s of history; inputs older than this are clamped *)

module Int_map = Map.Make (Int)

(* Everything the simulation knows about one frame [f]: [snap] is the state at
   the start of the frame and [inputs_1]/[inputs_2] what each player does
   during it, so the state at the start of [f + 1] is
   [step snap inputs_1 inputs_2]. *)
type frame_data = {
  snap : state;
  inputs_1 : player_input;
  inputs_2 : player_input;
}

(* One connected player. [last_seq] is the highest input sequence number
   applied so far, echoed back as [ack] so the client knows which of its
   predicted inputs the authoritative state already includes; [last_lag] the
   most recent measured round-trip in frames (for the ping log). *)
type seat = {
  conn : Websocket_lwt_unix.Connected_client.t;
  conn_id : int; (* connection number, only for the logs *)
  last_seq : int;
  last_lag : int;
}

(* Which player a connection controls. *)
type slot = P1 | P2

(* One hosted game. [frames] holds the [frame_data] of every frame in
   [frame - window .. frame] ([frame] being the latest simulated frame), which
   is what rollback can reach: inputs for older frames are clamped forward.
   [dirty_from] is the earliest frame whose input changed since the last tick
   and so needs replay. [running] is whether the simulation was ticking last
   frame (to log transitions and reset on pause); [last_points] the score at
   the previous tick (to log points as they are scored). *)
type game = {
  code : int;
  frame : int;
  frames : frame_data Int_map.t;
  dirty_from : int option;
  seat_1 : seat option;
  seat_2 : seat option;
  last_points : int * int;
  running : bool;
}

let player_number = function P1 -> 1 | P2 -> 2
let seat_of g = function P1 -> g.seat_1 | P2 -> g.seat_2

let with_seat g slot seat =
  match slot with
  | P1 -> { g with seat_1 = seat }
  | P2 -> { g with seat_2 = seat }

let inputs_of fd = function P1 -> fd.inputs_1 | P2 -> fd.inputs_2

let with_inputs fd slot input =
  match slot with
  | P1 -> { fd with inputs_1 = input }
  | P2 -> { fd with inputs_2 = input }

let is_full g = Option.is_some g.seat_1 && Option.is_some g.seat_2
let is_empty g = Option.is_none g.seat_1 && Option.is_none g.seat_2

let free_slot g =
  if Option.is_none g.seat_1 then Some P1
  else if Option.is_none g.seat_2 then Some P2
  else None

let initial_frames =
  Int_map.singleton 0
    { snap = initial_state; inputs_1 = no_input; inputs_2 = no_input }

let new_game code =
  {
    code;
    frame = 0;
    frames = initial_frames;
    dirty_from = None;
    seat_1 = None;
    seat_2 = None;
    last_points = (0, 0);
    running = false;
  }

(* Back to frame 0, ready for a fresh match; the connected seats stay but their
   input bookkeeping restarts (the clients also restart [seq] at 0 there). *)
let reset_sim g =
  let reset_seat =
    Option.map (fun seat -> { seat with last_seq = 0; last_lag = 0 })
  in
  {
    g with
    frame = 0;
    frames = initial_frames;
    dirty_from = None;
    last_points = (0, 0);
    running = false;
    seat_1 = reset_seat g.seat_1;
    seat_2 = reset_seat g.seat_2;
  }

(* The mutable core: the current value of each hosted game, keyed by its code,
   plus a counter naming connections in the logs. *)
let games : (int, game) Hashtbl.t = Hashtbl.create 16
let next_conn_id = ref 0

(* Apply the pure event [f] to the current value of game [code], if it still
   exists. *)
let update_game code f =
  match Hashtbl.find_opt games code with
  | Some g -> Hashtbl.replace games code (f g)
  | None -> ()

let log fmt = Printf.printf ("[server] " ^^ fmt ^^ "\n%!")
let to_client_msg m = Yojson.Safe.to_string (to_client_to_yojson m)

let fresh_code () =
  let rec go () =
    let code = 10_000 + Random.int 90_000 in
    if Hashtbl.mem games code then go () else code
  in
  go ()

(* Record [input] at the frame the client was reacting to (clamped to the
   rollback window) and mark the simulation for replay from there. The client
   sends several inputs per server frame (one per client frame, all tagged with
   the last server frame it has seen). Held direction is last-wins, but [jump]
   is a one-frame event, so we OR it in: a later [jump = false] from the same
   server frame must not erase a [jump = true] that already arrived. The
   carry-forward in [tick_game] clears jump on the next frame, so a press still
   fires exactly once. *)
let record_input g slot ~seq ~for_frame input =
  let f = max (g.frame - window) (min for_frame g.frame) in
  let g =
    match seat_of g slot with
    | None -> g
    | Some seat ->
        with_seat g slot
          (Some
             {
               seat with
               last_seq = seq;
               last_lag = max 0 (g.frame - for_frame);
             })
  in
  let frames =
    Int_map.update f
      (Option.map (fun fd ->
           let prev = inputs_of fd slot in
           with_inputs fd slot { input with jump = input.jump || prev.jump }))
      g.frames
  in
  let dirty_from =
    Some (match g.dirty_from with None -> f | Some d -> min d f)
  in
  { g with frames; dirty_from }

let handle_frame g slot (ws_frame : Websocket.Frame.t) =
  match ws_frame.opcode with
  | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary -> (
      match to_server_of_yojson (Yojson.Safe.from_string ws_frame.content) with
      | Ok { seq; for_frame; input } ->
          record_input g slot ~seq ~for_frame input
      | Error e ->
          log "game %05d: player %d: ignoring bad input json (%s)" g.code
            (player_number slot) e;
          g
      | exception exn ->
          log "game %05d: player %d: ignoring unparseable input (%s)" g.code
            (player_number slot) (Printexc.to_string exn);
          g)
  | _ -> g

let ms_of_frames f = int_of_float (Float.round (float_of_int f *. dt *. 1000.0))

(* One 60Hz tick of one game, as a pure map from the game to its next value
   plus the message to broadcast. With both players present: replay from the
   earliest changed frame (rollback), advance one new frame, emit the
   authoritative state. Otherwise: keep the simulation reset and tell whoever
   is there that they are waiting. *)
let tick_game g =
  if is_full g then begin
    if not g.running then
      log "game %05d: both players connected, simulation running" g.code;
    let start =
      match g.dirty_from with Some f -> min f g.frame | None -> g.frame
    in
    (* Replay [start .. frame]; the entry above the replayed one keeps its
       inputs and only gets its snapshot refreshed, except the brand-new
       current frame, which carries the held inputs forward (jump is momentary,
       so it never carries; it only ever applies on the frame it was
       pressed). [frames] covers [frame - window .. frame] and [start] is
       within the window by construction, so the [find] cannot fail. *)
    let rec replay frames f =
      if f > g.frame then frames
      else
        let fd = Int_map.find f frames in
        let snap = step ~dt ~input1:fd.inputs_1 ~input2:fd.inputs_2 fd.snap in
        let frames =
          Int_map.update (f + 1)
            begin function
              | Some fd' -> Some { fd' with snap }
              | None ->
                  Some
                    {
                      snap;
                      inputs_1 = { fd.inputs_1 with jump = false };
                      inputs_2 = { fd.inputs_2 with jump = false };
                    }
            end
            frames
        in
        replay frames (f + 1)
    in
    let frame = g.frame + 1 in
    let frames =
      Int_map.filter (fun f _ -> f >= frame - window) (replay g.frames start)
    in
    let s = (Int_map.find frame frames).snap in
    let points = (s.points1, s.points2) in
    let lag slot =
      match seat_of g slot with Some seat -> seat.last_lag | None -> 0
    in
    (* One log line per point scored, carrying the current pings so lag stays
       observable without flooding the log. *)
    if points <> g.last_points then
      log "game %05d: score %d - %d (ping %dms / %dms)" g.code (fst points)
        (snd points)
        (ms_of_frames (lag P1))
        (ms_of_frames (lag P2));
    let seq slot =
      match seat_of g slot with Some seat -> seat.last_seq | None -> 0
    in
    let msg =
      to_client_msg (State { frame; state = s; ack = (seq P1, seq P2) })
    in
    ( {
        g with
        frame;
        frames;
        dirty_from = None;
        last_points = points;
        running = true;
      },
      msg )
  end
  else begin
    if g.running then
      log "game %05d: a player left, simulation paused and reset" g.code;
    let g = if g.running then reset_sim g else g in
    (g, to_client_msg Waiting)
  end

let send_to client msg =
  Lwt.catch
    (fun () ->
      Websocket_lwt_unix.Connected_client.send client
        (Websocket.Frame.create ~content:msg ()))
    (fun _ -> Lwt.return_unit)

let broadcast g msg =
  Lwt.join
    (List.filter_map
       (Option.map (fun seat -> send_to seat.conn msg))
       [ g.seat_1; g.seat_2 ])

let close_client client =
  Lwt.catch
    (fun () ->
      Websocket_lwt_unix.Connected_client.send client
        (Websocket.Frame.close 1000))
    (fun _ -> Lwt.return_unit)

(* Refuse a connection: send [msg] (e.g. [Full]) and close politely. *)
let refuse client msg =
  let* () = send_to client (to_client_msg msg) in
  close_client client

(* A player sits down at [slot] of game [code]: welcome them, then pump their
   input frames into the simulation until they disconnect. Inputs are ignored
   while the game is not full (the simulation is paused and reset then). *)
let attach client ~id ~code slot =
  update_game code (fun g ->
      with_seat g slot
        (Some { conn = client; conn_id = id; last_seq = 0; last_lag = 0 }));
  log "player %d joined game %05d (connection #%d)" (player_number slot) code id;
  let* () =
    send_to client
      (to_client_msg (Welcome { player = player_number slot; code }))
  in
  let release () =
    update_game code (fun g -> with_seat g slot None);
    log "player %d left game %05d (connection #%d)" (player_number slot) code id;
    match Hashtbl.find_opt games code with
    | Some g when is_empty g ->
        Hashtbl.remove games code;
        log "game %05d closed" code
    | Some _ | None -> ()
  in
  let apply ws_frame g =
    (* Guard against a stale connection: only the seat's current owner may
       drive it, and only while the game is full. *)
    match seat_of g slot with
    | Some seat when seat.conn_id = id && is_full g ->
        handle_frame g slot ws_frame
    | Some _ | None -> g
  in
  let rec loop () =
    let* ws_frame = Websocket_lwt_unix.Connected_client.recv client in
    match ws_frame.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Close ->
        release ();
        Lwt.return_unit
    | _ ->
        with_lag (fun () ->
            update_game code (apply ws_frame);
            Lwt.return_unit);
        loop ()
  in
  Lwt.catch loop (fun _ ->
      release ();
      Lwt.return_unit)

let parse_hello (ws_frame : Websocket.Frame.t) =
  match ws_frame.opcode with
  | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary -> (
      match hello_of_yojson (Yojson.Safe.from_string ws_frame.content) with
      | Ok h -> Some h
      | Error _ | (exception _) -> None)
  | _ -> None

let handler client =
  let id = !next_conn_id in
  incr next_conn_id;
  Lwt.catch
    begin fun () ->
      (* The first message must be a [hello] choosing which game to enter. *)
      let* first = Websocket_lwt_unix.Connected_client.recv client in
      match parse_hello first with
      | Some Create ->
          let code = fresh_code () in
          Hashtbl.replace games code (new_game code);
          log "game %05d created (connection #%d)" code id;
          attach client ~id ~code P1
      | Some (Join code) -> (
          match Hashtbl.find_opt games code with
          | None ->
              log "connection #%d refused: no game %05d" id code;
              refuse client Unknown_game
          | Some g -> (
              match free_slot g with
              | None ->
                  log "connection #%d refused: game %05d is full" id code;
                  refuse client Full
              | Some slot -> attach client ~id ~code slot))
      | None ->
          log "connection #%d: bad hello, closing" id;
          close_client client
    end
    (fun _ -> Lwt.return_unit)

(* Tick every game once: swap in the new game values, then send the broadcasts
   (off the tick path, and with the outbound artificial delay). *)
let tick_all () =
  let ticked =
    Hashtbl.fold (fun code g acc -> (code, tick_game g) :: acc) games []
  in
  List.iter
    (fun (code, (g, msg)) ->
      Hashtbl.replace games code g;
      with_lag (fun () -> broadcast g msg))
    ticked

(* Tick on an absolute schedule rather than [sleep dt] (whose overshoot would
   make us run below 60Hz and desync from the 60fps clients). *)
let rec tick deadline =
  let deadline = deadline +. dt in
  let now = Unix.gettimeofday () in
  (* If we fell badly behind, resync instead of bursting to catch up. *)
  let deadline = if deadline < now -. 0.25 then now +. dt else deadline in
  let* () = Lwt_unix.sleep (max 0.0 (deadline -. now)) in
  tick_all ();
  tick deadline

(* Best-effort discovery of the LAN IP other machines should connect to: open a
   UDP socket "towards" an external address (no packet is actually sent — connect
   only picks the outgoing interface) and read back its local address. *)
let local_ip () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close s with _ -> ())
    (fun () ->
      Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_of_string "8.8.8.8", 80));
      match Unix.getsockname s with
      | Unix.ADDR_INET (addr, _) -> Some (Unix.string_of_inet_addr addr)
      | _ -> None)

let () =
  Random.self_init ();
  log "listening on port %d (all interfaces)" port;
  log "  this machine:   ws://localhost:%d" port;
  (match try local_ip () with _ -> None with
  | Some ip -> log "  on the network: ws://%s:%d" ip port
  | None -> log "  (could not determine LAN IP; use this machine's address)");
  let server =
    Websocket_lwt_unix.establish_server
      ~check_request:(fun _ -> true)
      ~mode:(`TCP (`Port port))
      handler
  in
  Lwt_main.run (Lwt.join [ server; tick (Unix.gettimeofday ()) ])
