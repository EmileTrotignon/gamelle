open Lwt.Syntax
open Libvolley

(* Authoritative volley server with rollback, hosting many games at once.

   Each game is identified by a random 5-digit code. A client's first message
   picks a game: [Create] opens a fresh one (the client becomes player 1 and is
   told the code to share), [Join code] takes the remaining slot of an existing
   one. A full game answers [Full]; an unknown code answers [Unknown_game].

   The server owns each simulation and ticks it at a fixed 60fps. Instead of
   applying "whatever input arrived last" at each tick (which lands inputs at
   the wrong sim-time and feels jittery), it keeps a ~1 second ring buffer of
   every frame's state and inputs. Each client tags its input with the frame it
   was reacting to; when that input arrives (necessarily a little late) the
   server inserts it at that frame and replays the simulation forward to the
   present. This makes the simulation consistent regardless of network jitter,
   never drops a single-frame jump, and gives us round-trip time for free.

   A game only simulates while both players are present; with a single player
   it stays reset and sends [Waiting] each tick, and it is deleted once empty.
   Everything runs in a single Lwt domain, so the shared mutable state below
   needs no locking. *)

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
        let open Lwt.Syntax in
        let* () = Lwt_unix.sleep half_lag in
        f ())
  else Lwt.async f

let window = 60 (* keep ~1s of history; inputs older than this are clamped *)
let n = 128 (* ring buffer size, comfortably larger than [window] *)

(* One hosted game. [snap.(f mod n)] is the state at the start of frame [f];
   [inp.(f mod n)] are the two players' inputs applied during frame [f] (so
   [snap.(f+1) = step snap.(f) inp.(f)]). [frame] is the latest simulated
   frame. [dirty_from] is the earliest frame whose input changed since the last
   tick and so needs replay. [last_lag] is the most recent measured round-trip
   per player in frames (for the ping log); [last_seq] the highest input [seq]
   applied per player, echoed back as [ack] so clients know which of their
   predicted inputs the authoritative state already includes. [players.(slot)]
   is the connection controlling player [slot + 1], and [running] whether the
   simulation was ticking last frame (to log transitions and reset on pause). *)
type game = {
  code : int;
  snap : state array;
  inp : player_input array array;
  mutable frame : int;
  mutable dirty_from : int option;
  last_lag : int array;
  last_seq : int array;
  players : Websocket_lwt_unix.Connected_client.t option array;
  mutable last_points : int * int;
  mutable running : bool;
}

let games : (int, game) Hashtbl.t = Hashtbl.create 16
let next_id = ref 0
let log fmt = Printf.printf ("[server] " ^^ fmt ^^ "\n%!")
let to_client_msg m = Yojson.Safe.to_string (to_client_to_yojson m)

let string_of_input { left; right; down; jump } =
  Printf.sprintf "left=%b right=%b down=%b jump=%b" left right down jump

let new_game () =
  let rec fresh_code () =
    let code = 10_000 + Random.int 90_000 in
    if Hashtbl.mem games code then fresh_code () else code
  in
  let code = fresh_code () in
  let g =
    {
      code;
      snap = Array.make n initial_state;
      inp = Array.init n (fun _ -> [| no_input; no_input |]);
      frame = 0;
      dirty_from = None;
      last_lag = [| 0; 0 |];
      last_seq = [| 0; 0 |];
      players = [| None; None |];
      last_points = (0, 0);
      running = false;
    }
  in
  Hashtbl.replace games code g;
  g

let is_full g = Array.for_all Option.is_some g.players

let free_slot g =
  if Option.is_none g.players.(0) then Some 0
  else if Option.is_none g.players.(1) then Some 1
  else None

let reset_sim g =
  Array.fill g.snap 0 n initial_state;
  Array.iter
    (fun a ->
      a.(0) <- no_input;
      a.(1) <- no_input)
    g.inp;
  g.frame <- 0;
  g.dirty_from <- None;
  g.last_points <- (0, 0);
  g.last_lag.(0) <- 0;
  g.last_lag.(1) <- 0;
  g.last_seq.(0) <- 0;
  g.last_seq.(1) <- 0

let send_to client msg =
  Lwt.catch
    (fun () ->
      Websocket_lwt_unix.Connected_client.send client
        (Websocket.Frame.create ~content:msg ()))
    (fun _ -> Lwt.return_unit)

let broadcast g msg =
  Array.fold_left
    (fun acc player ->
      match player with None -> acc | Some c -> send_to c msg :: acc)
    [] g.players
  |> Lwt.join

let mark_dirty g f =
  g.dirty_from <- Some (match g.dirty_from with None -> f | Some d -> min d f)

(* Record [input] for [slot] at the frame the client was reacting to, then mark
   the simulation for replay from there. Logs ping (round-trip in ms) and input
   changes; held input is resent every frame so we only log it when it changes. *)
let record_input g slot ~seq ~for_frame input =
  g.last_seq.(slot) <- seq;
  g.last_lag.(slot) <- max 0 (g.frame - for_frame);
  let f = max (g.frame - window) (min for_frame g.frame) in
  let prev = g.inp.(f mod n).(slot) in
  (* The client sends several inputs per server frame (one per client frame, all
     tagged with the last server frame it has seen). Held direction is last-wins,
     but [jump] is a one-frame event, so we OR it in: a later [jump=false] from
     the same server frame must not erase a [jump=true] that already arrived. The
     carry-forward in [tick] clears jump on the next frame, so a press still fires
     exactly once. *)
  let merged = { input with jump = input.jump || prev.jump } in
  if input.jump && not prev.jump then
    log "game %05d: player %d jump @%d" g.code (slot + 1) f
  else if { input with jump = false } <> { prev with jump = false } then
    log "game %05d: player %d input @%d: %s" g.code (slot + 1) f
      (string_of_input merged);
  g.inp.(f mod n).(slot) <- merged;
  mark_dirty g f

let handle_frame g slot (ws_frame : Websocket.Frame.t) =
  match ws_frame.opcode with
  | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary -> (
      match to_server_of_yojson (Yojson.Safe.from_string ws_frame.content) with
      | Ok { seq; for_frame; input } -> record_input g slot ~seq ~for_frame input
      | Error e ->
          log "game %05d: player %d: ignoring bad input json (%s)" g.code
            (slot + 1) e
      | exception exn ->
          log "game %05d: player %d: ignoring unparseable input (%s)" g.code
            (slot + 1) (Printexc.to_string exn))
  | _ -> ()

let close_client client =
  Lwt.catch
    (fun () ->
      Websocket_lwt_unix.Connected_client.send client (Websocket.Frame.close 1000))
    (fun _ -> Lwt.return_unit)

(* Refuse a connection: send [msg] (e.g. [Full]) and close politely. *)
let refuse client msg =
  let* () = send_to client (to_client_msg msg) in
  close_client client

(* A player is attached to game [g] at [slot]: welcome them, then pump their
   input frames into the simulation until they disconnect. Inputs are ignored
   while the game is not full (the simulation is paused and reset then). *)
let attach client ~id g slot =
  g.players.(slot) <- Some client;
  log "player %d joined game %05d (connection #%d)" (slot + 1) g.code id;
  let* () =
    send_to client (to_client_msg (Welcome { player = slot + 1; code = g.code }))
  in
  let release () =
    g.players.(slot) <- None;
    g.last_lag.(slot) <- 0;
    g.last_seq.(slot) <- 0;
    log "player %d left game %05d (connection #%d)" (slot + 1) g.code id;
    if Array.for_all Option.is_none g.players then begin
      Hashtbl.remove games g.code;
      log "game %05d closed" g.code
    end
  in
  let rec loop () =
    let* ws_frame = Websocket_lwt_unix.Connected_client.recv client in
    match ws_frame.Websocket.Frame.opcode with
    | Websocket.Frame.Opcode.Close ->
        release ();
        Lwt.return_unit
    | _ ->
        with_lag (fun () ->
            if is_full g then handle_frame g slot ws_frame;
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
  let id = !next_id in
  incr next_id;
  Lwt.catch
    (fun () ->
      (* The first message must be a [hello] choosing which game to enter. *)
      let* first = Websocket_lwt_unix.Connected_client.recv client in
      match parse_hello first with
      | Some Create ->
          let g = new_game () in
          log "game %05d created (connection #%d)" g.code id;
          attach client ~id g 0
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
              | Some slot -> attach client ~id g slot))
      | None ->
          log "connection #%d: bad hello, closing" id;
          close_client client)
    (fun _ -> Lwt.return_unit)

let ms_of_frames f = int_of_float (Float.round (float_of_int f *. dt *. 1000.0))

(* One 60Hz tick of one game. With both players present: replay from the
   earliest changed frame (rollback), advance one new frame, broadcast the
   authoritative state. Otherwise: keep the simulation reset and tell whoever
   is there that they are waiting. *)
let tick_game g =
  if is_full g then begin
    if not g.running then begin
      g.running <- true;
      log "game %05d: both players connected, simulation running" g.code
    end;
    let start =
      match g.dirty_from with Some f -> min f g.frame | None -> g.frame
    in
    g.dirty_from <- None;
    for f = start to g.frame do
      g.snap.((f + 1) mod n) <-
        step ~dt
          ~input1:g.inp.(f mod n).(0)
          ~input2:g.inp.(f mod n).(1)
          g.snap.(f mod n)
    done;
    g.frame <- g.frame + 1;
    (* Carry held inputs forward to the new current frame (jump is momentary, so
       it never carries; it only ever applies on the frame it was pressed). *)
    let prev = (g.frame - 1) mod n and cur = g.frame mod n in
    g.inp.(cur).(0) <- { (g.inp.(prev).(0)) with jump = false };
    g.inp.(cur).(1) <- { (g.inp.(prev).(1)) with jump = false };
    let s = g.snap.(g.frame mod n) in
    let pts = (s.points1, s.points2) in
    if pts <> g.last_points then begin
      g.last_points <- pts;
      log "game %05d: score: player 1 = %d, player 2 = %d" g.code (fst pts)
        (snd pts)
    end;
    (* Ping report once a second. *)
    if g.frame mod 60 = 0 then
      log "game %05d: ping: player 1 = %dms, player 2 = %dms" g.code
        (ms_of_frames g.last_lag.(0))
        (ms_of_frames g.last_lag.(1));
    let msg =
      to_client_msg
        (State
           { frame = g.frame; state = s; ack = (g.last_seq.(0), g.last_seq.(1)) })
    in
    (* Don't block the tick on the network; apply the outbound artificial delay. *)
    with_lag (fun () -> broadcast g msg)
  end
  else begin
    if g.running then begin
      g.running <- false;
      reset_sim g;
      log "game %05d: a player left, simulation paused and reset" g.code
    end;
    with_lag (fun () -> broadcast g (to_client_msg Waiting))
  end

(* Tick on an absolute schedule rather than [sleep dt] (whose overshoot would
   make us run below 60Hz and desync from the 60fps clients). *)
let next_deadline = ref 0.0

let rec tick () =
  if !next_deadline = 0.0 then next_deadline := Unix.gettimeofday ();
  next_deadline := !next_deadline +. dt;
  let now = Unix.gettimeofday () in
  (* If we fell badly behind, resync instead of bursting to catch up. *)
  if !next_deadline < now -. 0.25 then next_deadline := now +. dt;
  let* () = Lwt_unix.sleep (max 0.0 (!next_deadline -. now)) in
  Hashtbl.iter (fun _ g -> tick_game g) games;
  tick ()

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
  Lwt_main.run (Lwt.join [ server; tick () ])
