open Brr
open Brr_webaudio
open Gamelle_common
open Geometry
module Text = Text
module Color = Color
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
include Draw
include Jsoo

module Net = struct
  (* The browser WebSocket API is event-based: [onmessage] fires on the JS event
     loop between animation frames. We just buffer incoming text frames in a
     queue that [poll] drains; no Lwt or promises are involved. *)
  type t = { ws : Brr_io.Websocket.t; recv : string Queue.t }

  let connect url =
    let ws = Brr_io.Websocket.create (Jstr.of_string url) in
    let recv = Queue.create () in
    let _listener : Ev.listener =
      Ev.listen Brr_io.Message.Ev.message
        (fun ev ->
          let data : Jstr.t = Brr_io.Message.Ev.data (Ev.as_type ev) in
          Queue.add (Jstr.to_string data) recv)
        (Brr_io.Websocket.as_target ws)
    in
    { ws; recv }

  let send t msg =
    (* [send] throws if the socket is not open yet, so drop until it is. *)
    if Brr_io.Websocket.ready_state t.ws = Brr_io.Websocket.Ready_state.open'
    then Brr_io.Websocket.send_string t.ws (Jstr.of_string msg)

  let poll t =
    let rec drain acc =
      if Queue.is_empty t.recv then List.rev acc
      else drain (Queue.pop t.recv :: acc)
    in
    drain []

  let close t = Brr_io.Websocket.close t.ws
end

let prev_now = ref 0.0
let now = ref 0.0
let clock = Gamelle_common.clock
let dt = Gamelle_common.dt

module Window = struct
  let current_size = ref (0, 0)

  let set_size ~io =
    let s = !(io.window_size) in
    if s <> (0, 0) && s <> !current_size then begin
      let w, h = s in
      let canvas = io.backend.canvas in
      Canvas.set_w canvas w;
      Canvas.set_h canvas h;
      current_size := s
    end

  let size ~io =
    let canvas = io.backend.canvas in
    Size.v (float (Canvas.w canvas)) (float (Canvas.h canvas))

  let show_cursor ~io status =
    let canvas = io.backend.canvas in
    let el = Canvas.to_el canvas in
    if status then Brr.El.remove_inline_style Brr.El.Style.cursor el
    else Brr.El.set_inline_style Brr.El.Style.cursor (Jstr.of_string "none") el

  let set_fullscreen ~io fullscreen =
    let canvas = io.backend.canvas in
    let el = Canvas.to_el canvas in
    let _fut =
      if fullscreen then El.request_fullscreen el
      else Document.exit_fullscreen G.document
    in
    ()

  let get_fullscreen ~io:_ =
    Option.is_some (Document.fullscreen_element G.document)
end

let finalize_frame ~io =
  Sound.end_frame ~io;
  Window.set_size ~io;
  let ctx = io.backend.ctx in
  let w, h = !Window.current_size in
  C.reset_transform ctx;
  C.set_fill_style ctx (C.color (Jstr.of_string "black"));
  C.fill_rect ctx ~x:0.0 ~y:0.0 ~w:(float w) ~h:(float h);
  Gamelle_common.finalize_frame ~io

let run ~canvas state update =
  let open Jsoo in
  Events_js.attach ~target:(El.as_target canvas);
  let canvas = Canvas.of_el canvas in
  Canvas.set_w canvas (640 * 2);
  Canvas.set_h canvas (480 * 2);

  let ctx = C.get_context canvas in
  let audio = Audio.Context.create () in

  let backend =
    { canvas; ctx; audio; font = Font.default; font_size = Font.default_size }
  in
  let io = make_io backend in
  let clock_ref = ref 0 in

  let rec animate state =
    let _ = G.request_animation_frame (loop state) in
    ()
  and loop state elapsed =
    let open Events_backend in
    prev_now := !now;
    now := elapsed /. 1000.0;
    Events_js.new_frame ();
    io_reset_mutable_fields io;
    io.event := { !Events_js.current with clock = !clock_ref };
    incr clock_ref;
    let state = update ~io state in
    finalize_frame ~io;
    Events_js.current := reset_wheel !Events_js.current;
    animate state
  in
  animate state

let run state update =
  let canvas =
    match Document.find_el_by_id G.document (Jstr.of_string "target") with
    | None -> failwith "missing 'target' canvas"
    | Some elt -> elt
  in
  let started = ref false in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "focus"))
      begin fun _ ->
        if !started then ()
        else begin
          started := true;
          run ~canvas state update
        end
      end
      (El.as_target canvas)
  in
  El.set_has_focus true canvas;
  ()
