open Lwt.Syntax
open Libvolley

(* Authoritative volley server with rollback.

   The server owns the simulation and ticks it at a fixed 60fps. Instead of
   applying "whatever input arrived last" at each tick (which lands inputs at the
   wrong sim-time and feels jittery), it keeps a ~1 second ring buffer of every
   frame's state and inputs. Each client tags its input with the frame it was
   reacting to; when that input arrives (necessarily a little late) the server
   inserts it at that frame and replays the simulation forward to the present.
   This makes the simulation consistent regardless of network jitter, never
   drops a single-frame jump, and gives us round-trip time for free.

   The first two clients to connect become player 1 and 2; further connections
   are refused (no spectators). Everything runs in a single Lwt domain, so the
   shared mutable state below needs no locking. *)

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

(* [snap.(f mod n)] is the state at the start of frame [f]; [inp.(f mod n)] are
   the two players' inputs applied during frame [f] (so [snap.(f+1) = step snap.(f)
   inp.(f)]). [frame] is the latest simulated frame. *)
let snap = Array.make n initial_state
let inp = Array.init n (fun _ -> [| no_input; no_input |])
let frame = ref 0

(* Earliest frame whose input changed since the last tick and so needs replay. *)
let dirty_from = ref None

(* Most recent measured round-trip lag per player, in frames (for the ping log). *)
let last_lag = [| 0; 0 |]

(* Highest input [seq] applied per player, echoed back as [ack] so clients know
   which of their predicted inputs the authoritative state already includes. *)
let last_seq = [| 0; 0 |]

let clients : (int, Websocket_lwt_unix.Connected_client.t) Hashtbl.t =
  Hashtbl.create 4

let next_id = ref 0
let free_slots = ref [ 0; 1 ]
let log fmt = Printf.printf ("[server] " ^^ fmt ^^ "\n%!")
let to_client_msg m = Yojson.Safe.to_string (to_client_to_yojson m)

let string_of_input { left; right; down; jump } =
  Printf.sprintf "left=%b right=%b down=%b jump=%b" left right down jump

let send_to client msg =
  Lwt.catch
    (fun () ->
      Websocket_lwt_unix.Connected_client.send client
        (Websocket.Frame.create ~content:msg ()))
    (fun _ -> Lwt.return_unit)

let broadcast msg =
  Hashtbl.fold (fun _ client acc -> send_to client msg :: acc) clients []
  |> Lwt.join

let mark_dirty g =
  dirty_from := Some (match !dirty_from with None -> g | Some d -> min d g)

(* Record [input] for [slot] at the frame the client was reacting to, then mark
   the simulation for replay from there. Logs ping (round-trip in ms) and input
   changes; held input is resent every frame so we only log it when it changes. *)
let record_input slot ~seq ~for_frame input =
  last_seq.(slot) <- seq;
  last_lag.(slot) <- max 0 (!frame - for_frame);
  let g = max (!frame - window) (min for_frame !frame) in
  let prev = inp.(g mod n).(slot) in
  (* The client sends several inputs per server frame (one per client frame, all
     tagged with the last server frame it has seen). Held direction is last-wins,
     but [jump] is a one-frame event, so we OR it in: a later [jump=false] from
     the same server frame must not erase a [jump=true] that already arrived. The
     carry-forward in [tick] clears jump on the next frame, so a press still fires
     exactly once. *)
  let merged = { input with jump = input.jump || prev.jump } in
  if input.jump && not prev.jump then log "player %d jump @%d" (slot + 1) g
  else if { input with jump = false } <> { prev with jump = false } then
    log "player %d input @%d: %s" (slot + 1) g (string_of_input merged);
  inp.(g mod n).(slot) <- merged;
  mark_dirty g

let handle_frame slot (ws_frame : Websocket.Frame.t) =
  match ws_frame.opcode with
  | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary -> (
      match to_server_of_yojson (Yojson.Safe.from_string ws_frame.content) with
      | Ok { seq; for_frame; input } -> record_input slot ~seq ~for_frame input
      | Error e -> log "player %d: ignoring bad input json (%s)" (slot + 1) e
      | exception exn ->
          log "player %d: ignoring unparseable input (%s)" (slot + 1)
            (Printexc.to_string exn))
  | _ -> ()

let handler client =
  let id = !next_id in
  incr next_id;
  match !free_slots with
  | [] ->
      log "connection #%d refused: both players are already connected" id;
      let* () = send_to client (to_client_msg Full) in
      Lwt.catch
        (fun () ->
          Websocket_lwt_unix.Connected_client.send client
            (Websocket.Frame.close 1000))
        (fun _ -> Lwt.return_unit)
  | slot :: rest ->
      free_slots := rest;
      Hashtbl.replace clients id client;
      log "player %d connected (connection #%d)" (slot + 1) id;
      let* () = send_to client (to_client_msg (Welcome (slot + 1))) in
      let release () =
        Hashtbl.remove clients id;
        free_slots := slot :: !free_slots;
        last_lag.(slot) <- 0;
        last_seq.(slot) <- 0;
        log "player %d disconnected (connection #%d)" (slot + 1) id
      in
      let rec loop () =
        let* ws_frame = Websocket_lwt_unix.Connected_client.recv client in
        match ws_frame.Websocket.Frame.opcode with
        | Websocket.Frame.Opcode.Close ->
            release ();
            Lwt.return_unit
        | _ ->
            with_lag (fun () -> handle_frame slot ws_frame; Lwt.return_unit);
            loop ()
      in
      Lwt.catch loop (fun _ ->
          release ();
          Lwt.return_unit)

let last_points = ref (0, 0)
let was_idle = ref true

let reset_sim () =
  Array.fill snap 0 n initial_state;
  Array.iter (fun a -> a.(0) <- no_input; a.(1) <- no_input) inp;
  frame := 0;
  dirty_from := None;
  last_points := (0, 0);
  last_lag.(0) <- 0;
  last_lag.(1) <- 0;
  last_seq.(0) <- 0;
  last_seq.(1) <- 0

let ms_of_frames f = int_of_float (Float.round (float_of_int f *. dt *. 1000.0))

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
  if Hashtbl.length clients = 0 then begin
    (* No one is connected: pause the simulation entirely and reset so the next
       match starts fresh. *)
    if not !was_idle then begin
      log "no players connected, simulation paused";
      reset_sim ()
    end;
    was_idle := true;
    tick ()
  end
  else begin
    if !was_idle then log "players connected, simulation running";
    was_idle := false;
    (* Replay from the earliest changed frame (rollback), then advance one new
       frame. When nothing changed this is just a single forward step. *)
    let start = match !dirty_from with Some g -> min g !frame | None -> !frame in
    dirty_from := None;
    for f = start to !frame do
      snap.((f + 1) mod n) <-
        step ~dt ~input1:inp.(f mod n).(0) ~input2:inp.(f mod n).(1)
          snap.(f mod n)
    done;
    incr frame;
    (* Carry held inputs forward to the new current frame (jump is momentary, so
       it never carries; it only ever applies on the frame it was pressed). *)
    let prev = (!frame - 1) mod n and cur = !frame mod n in
    inp.(cur).(0) <- { inp.(prev).(0) with jump = false };
    inp.(cur).(1) <- { inp.(prev).(1) with jump = false };
    let s = snap.(!frame mod n) in
    let pts = (s.points1, s.points2) in
    if pts <> !last_points then begin
      last_points := pts;
      log "score: player 1 = %d, player 2 = %d" (fst pts) (snd pts)
    end;
    (* Ping report once a second. *)
    if !frame mod 60 = 0 then
      log "ping: player 1 = %dms, player 2 = %dms"
        (ms_of_frames last_lag.(0))
        (ms_of_frames last_lag.(1));
    let msg =
      to_client_msg
        (State { frame = !frame; state = s; ack = (last_seq.(0), last_seq.(1)) })
    in
    (* Don't block the tick on the network; apply the outbound artificial delay. *)
    with_lag (fun () -> broadcast msg);
    tick ()
  end

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
  log "listening on port %d (all interfaces)" port;
  log "  this machine:   ws://localhost:%d" port;
  (match (try local_ip () with _ -> None) with
  | Some ip -> log "  on the network: ws://%s:%d" ip port
  | None -> log "  (could not determine LAN IP; use this machine's address)");
  let server =
    Websocket_lwt_unix.establish_server
      ~check_request:(fun _ -> true)
      ~mode:(`TCP (`Port port)) handler
  in
  Lwt_main.run (Lwt.join [ server; tick () ])
