open Lwt.Syntax
open Libvolley

(* Authoritative volley server.

   It owns the game state, steps the simulation on a fixed 60fps tick and
   broadcasts the resulting state to every connected client. Clients only ever
   send their own [player_input]; the first two clients to connect become
   player 1 and player 2. Once both slots are taken, further connections are
   refused (no spectators).

   Everything runs in a single Lwt domain (one [Lwt_main.run]), so the shared
   mutable state below is touched from a single cooperative thread and needs no
   locking. *)

let port = 8080
let dt = 1.0 /. 60.0
let state = ref initial_state

(* Latest input received for player 1 (slot 0) and player 2 (slot 1). *)
let inputs = [| no_input; no_input |]

(* Connected clients, keyed by a fresh id per connection so a disconnect can
   remove exactly its own entry (and free its player slot). *)
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

(* Apply an input frame to player [slot], logging it only when it actually
   changes (clients resend their input every frame, which would be far too
   noisy to log otherwise). *)
let handle_frame slot (frame : Websocket.Frame.t) =
  match frame.opcode with
  | Websocket.Frame.Opcode.Text | Websocket.Frame.Opcode.Binary -> (
      match player_input_of_yojson (Yojson.Safe.from_string frame.content) with
      | Ok input ->
          if input <> inputs.(slot) then
            log "player %d input: %s" (slot + 1) (string_of_input input);
          inputs.(slot) <- input
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
        inputs.(slot) <- no_input;
        log "player %d disconnected (connection #%d)" (slot + 1) id
      in
      let rec loop () =
        let* frame = Websocket_lwt_unix.Connected_client.recv client in
        match frame.Websocket.Frame.opcode with
        | Websocket.Frame.Opcode.Close ->
            release ();
            Lwt.return_unit
        | _ ->
            handle_frame slot frame;
            loop ()
      in
      Lwt.catch loop (fun _ ->
          release ();
          Lwt.return_unit)

let last_points = ref (0, 0)
let was_idle = ref true

let rec tick () =
  let* () = Lwt_unix.sleep dt in
  if Hashtbl.length clients = 0 then begin
    (* No one is connected: pause the simulation entirely and reset so the next
       match starts fresh. *)
    if not !was_idle then log "no players connected, simulation paused";
    was_idle := true;
    state := initial_state;
    last_points := (0, 0);
    tick ()
  end
  else begin
    if !was_idle then log "players connected, simulation running";
    was_idle := false;
    let s = step ~dt ~input1:inputs.(0) ~input2:inputs.(1) !state in
    state := s;
    let pts = (s.points1, s.points2) in
    if pts <> !last_points then begin
      last_points := pts;
      log "score: player 1 = %d, player 2 = %d" (fst pts) (snd pts)
    end;
    let* () = broadcast (to_client_msg (State s)) in
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
