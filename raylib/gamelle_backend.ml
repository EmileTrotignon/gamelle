include Common
module Geometry = Gamelle_common.Geometry
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
module Text = Text
include Draw
module Window = Window

(* Headless screenshots are taken entirely outside the backend (see
   test/raylib_screenshot.sh): the program runs normally under Xvfb and the
   virtual framebuffer is captured externally. Audio init is skipped when no
   audio device is available (e.g. under Xvfb) so it does not fail there. *)
let has_audio =
  match Sys.getenv_opt "GAMELLE_NO_AUDIO" with Some _ -> false | None -> true

let run state update =
  Raylib.set_config_flags Raylib.ConfigFlags.(msaa_4x_hint + window_highdpi);
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  Raylib.init_window 640 640 "Gamelle";
  (* Raylib.begin_blend_mode Raylib.BlendMode.Alpha_premultiply; *)
  Raylib.set_target_fps 60;
  if has_audio then Raylib.init_audio_device ();

  let backend = { font = Font_.default; font_size = Font_.default_size } in
  let io = Gamelle_common.make_io backend in
  let dpi_scale = Raylib.Vector2.x (Raylib.get_window_scale_dpi ()) in
  let clock_ref = ref 0 in
  let state = ref state in

  let running = ref true in
  while !running do
    Raylib.begin_drawing ();
    let prev_event = !(io.event) in
    Gamelle_common.io_reset_mutable_fields io;
    io.event := Events_raylib.update !clock_ref prev_event;
    incr clock_ref;
    if Gamelle_common.Events_backend.is_pressed !(io.event) `quit then
      running := false;
    let io = { io with view = Transform.scale dpi_scale io.view } in
    state := update ~io !state;
    Window.finalize_frame ~io;
    if has_audio then Sound.update_current_music ();
    Raylib.end_drawing ()
  done;
  if has_audio then begin
    Sound.cleanup ();
    Raylib.close_audio_device ()
  end;
  Raylib.close_window ()

module Net = struct
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

  let push mutex q v =
    Mutex.lock mutex;
    Queue.add v q;
    Mutex.unlock mutex

  let drain mutex q =
    Mutex.lock mutex;
    let rec go acc =
      if Queue.is_empty q then acc else go (Queue.pop q :: acc)
    in
    let items = go [] in
    Mutex.unlock mutex;
    List.rev items

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
      Domain.spawn (fun () ->
          let open Lwt.Syntax in
          let main =
            Lwt.catch
              (fun () ->
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
                  Resolver_lwt.resolve_uri ~uri:resolve_uri
                    Resolver_lwt_unix.system
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
                let* conn =
                  Websocket_lwt_unix.connect ~random_string client uri
                in
                Atomic.set t.status Connected;
                let rec recv_loop () =
                  let* frame = Websocket_lwt_unix.read conn in
                  match frame.Websocket.Frame.opcode with
                  | Websocket.Frame.Opcode.Close ->
                      Websocket_lwt_unix.close_transport conn
                  | _ ->
                      push t.recv_mutex t.recv frame.Websocket.Frame.content;
                      recv_loop ()
                in
                let rec send_loop () =
                  if t.closed then Websocket_lwt_unix.close_transport conn
                  else
                    let* () =
                      Lwt_list.iter_s
                        (fun content ->
                          Websocket_lwt_unix.write conn
                            (Websocket.Frame.create ~content ()))
                        (drain t.send_mutex t.send_q)
                    in
                    let* () = Lwt_unix.sleep 0.002 in
                    send_loop ()
                in
                Lwt.pick [ recv_loop (); send_loop () ])
              (fun exn ->
                Atomic.set t.status (Error (Printexc.to_string exn));
                Lwt.return_unit)
          in
          Lwt_main.run main;
          (* A clean exit (server closed, or [close] was requested) leaves the
             status at [Connecting]/[Connected]; only override when no error was
             recorded. *)
          match Atomic.get t.status with
          | Error _ -> ()
          | _ -> Atomic.set t.status Closed)
    in
    t

  let send t msg = push t.send_mutex t.send_q msg
  let poll t = drain t.recv_mutex t.recv
  let status t = Atomic.get t.status
  let is_connected t = Atomic.get t.status = Connected
  let close t = t.closed <- true
end
