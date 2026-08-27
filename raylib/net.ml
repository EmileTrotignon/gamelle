(* The websocket runs on its own domain with its own [Lwt_main.run]. All Lwt
   activity stays inside that domain (Lwt is not multicore-safe); the only
   contact with the game domain is through two mutex-protected queues, which is
   enough for the game to [send]/[poll] without ever blocking on the network.
*)

type status = Connecting | Connected | Closed | Error of string

type t = {
  recv : string Queue.t;
  recv_mutex : Mutex.t;
  send_q : string Queue.t;
  send_mutex : Mutex.t;
  mutable closed : bool;
  (* Written from the network domain, read from the game domain. *)
  status : status Atomic.t;
}

let status_to_string = function
  | Connecting -> "connecting"
  | Connected -> "connected"
  | Closed -> "closed"
  | Error msg -> "error: " ^ msg

let push mutex q v = Mutex.protect mutex (fun () -> Queue.add v q)

let drain mutex q =
  Mutex.protect mutex begin fun () ->
      let rec go acc =
        if Queue.is_empty q then acc else go (Queue.pop q :: acc)
      in
      let items = go [] in
      List.rev items
    end

let connect url =
  let t =
    {
      recv = Queue.create ();
      recv_mutex = Mutex.create ();
      send_q = Queue.create ();
      send_mutex = Mutex.create ();
      closed = false;
      status = Atomic.make Connecting;
    }
  in
  let _ : unit Domain.t =
    Domain.spawn @@ fun () ->
    let open Lwt.Syntax in
    let main =
      Lwt.catch
        begin fun () ->
          let uri = Uri.of_string url in
          (* conduit's resolver does not know the [ws]/[wss] schemes, so
                 resolve as [http]/[https] while keeping the original [ws] uri
                 for the websocket handshake itself. *)
          let resolve_uri =
            match Uri.scheme uri with
            | Some "ws" -> Uri.with_scheme uri (Some "http")
            | Some "wss" -> Uri.with_scheme uri (Some "https")
            | _ -> uri
          in
          let* endp =
            Resolver_lwt.resolve_uri ~uri:resolve_uri Resolver_lwt_unix.system
          in
          let* client =
            Conduit_lwt_unix.endp_to_client
              ~ctx:(Lazy.force Conduit_lwt_unix.default_ctx)
              endp
          in
          (* The default [Sec-WebSocket-Key] generator pulls from
                 mirage-crypto-rng, which would need separate initialisation;
                 a game handshake key needs no crypto-grade randomness, so
                 supply our own. *)
          let random_string n =
            String.init n (fun _ -> Char.chr (Random.int 256))
          in
          let* conn = Gamelle_websocket.connect ~random_string client uri in
          Atomic.set t.status Connected;
          let rec recv_loop () =
            let* frame = Gamelle_websocket.read conn in
            let open Websocket.Frame in
            match frame.opcode with
            | Opcode.Close -> Gamelle_websocket.close_transport conn
            | Opcode.Ping ->
                (* Control frame: answer it, don't surface it as a game
                       message. *)
                let* () =
                  Gamelle_websocket.write conn
                    (create ~opcode:Opcode.Pong ~content:frame.content ())
                in
                recv_loop ()
            | Opcode.Pong -> recv_loop ()
            | Opcode.Text | Opcode.Binary | Opcode.Continuation ->
                push t.recv_mutex t.recv frame.content;
                recv_loop ()
            | Opcode.Ctrl _ | Opcode.Nonctrl _ -> recv_loop ()
          in
          let rec send_loop () =
            if t.closed then Gamelle_websocket.close_transport conn
            else
              let* () =
                Lwt_list.iter_s
                  (fun content ->
                    Gamelle_websocket.write conn
                      (Websocket.Frame.create ~content ()))
                  (drain t.send_mutex t.send_q)
              in
              let* () = Lwt_unix.sleep 0.002 in
              send_loop ()
          in
          Lwt.pick [ recv_loop (); send_loop () ]
        end begin fun exn ->
        Atomic.set t.status (Error (Printexc.to_string exn));
        Lwt.return_unit
        end
    in
    Lwt_main.run main;
    (* A clean exit (server closed, or [close] was requested) leaves the
           status at [Connecting]/[Connected]; only override when no error was
           recorded. *)
    match Atomic.get t.status with
    | Error _ -> ()
    | _ -> Atomic.set t.status Closed
  in
  t

let send t msg =
  match Atomic.get t.status with
  | Connected -> push t.send_mutex t.send_q msg
  | (Connecting | Closed | Error _) as st ->
      failwith
      @@ Printf.sprintf
           "Net.send: the connection is not open (%s); check Net.status before \
            sending"
           (status_to_string st)

let poll t = drain t.recv_mutex t.recv
let status t = Atomic.get t.status
let is_connected t = Atomic.get t.status = Connected
let close t = t.closed <- true
