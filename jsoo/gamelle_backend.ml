open Brr
open Brr_webaudio
open Gamelle_common
open Geometry
module Text = Text
module Color = Color
module Bitmap = Bitmap
module Svg = Svg
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
include Draw
include Jsoo
module Net = Net

(* Active (visible) time, in seconds, at which the previous frame ran. *)
let prev_now = ref 0.0

(* The browser stops firing [request_animation_frame] while the tab is hidden,
   so the [elapsed] timestamp jumps by the whole pause on the frame we resume.
   To keep [dt] a real measure of the time the game was actually running, we
   track how long the tab has been hidden and subtract it: [hidden_total] is the
   accumulated hidden time and [hidden_at] the timestamp we became hidden (both
   in seconds, on the same [performance.now] clock as [elapsed]). *)
let hidden_total = ref 0.0
let hidden_at = ref None

(* [true] once the first frame has run; the first frame has no predecessor to
   measure against, so it falls back to [target_dt]. *)
let started = ref false
let clock = Gamelle_common.clock
let dt = Gamelle_common.dt

module Window = struct
  let px v = Jstr.of_string (string_of_int v ^ "px")

  (* Synchronize the canvas with the game's logical size: the element is
     displayed at the logical size (in fullscreen the UA stylesheet overrides
     this and stretches it to the screen), while the backing store follows the
     element's real on-screen resolution so rendering stays sharp. Runs every
     frame; the canvas attributes are only written on change (writing them
     resets the canvas). *)
  let set_size ~io =
    let s = !(io.window_size) in
    if s <> (0, 0) then Jsoo.logical_size := s;
    let lw, lh = !Jsoo.logical_size in
    let canvas = io.backend.canvas in
    let el = Canvas.to_el canvas in
    El.set_inline_style El.Style.width (px lw) el;
    El.set_inline_style El.Style.height (px lh) el;
    (* When fullscreen stretches the element to a screen with a different
       aspect ratio, display the bitmap letterboxed at its own aspect ratio,
       centered, with black bars. *)
    El.set_inline_style (Jstr.v "object-fit") (Jstr.v "contain") el;
    El.set_inline_style (Jstr.v "background-color") (Jstr.v "black") el;
    let cw = El.inner_w el and ch = El.inner_h el in
    (* A detached or hidden canvas measures 0: fall back to the logical size. *)
    let cw = if cw > 0. then cw else float lw in
    let ch = if ch > 0. then ch else float lh in
    (* Size the backing store to the letterboxed display rectangle: the
       largest logical-aspect rectangle fitting the element. [object-fit:
       contain] then shows it 1:1, so rendering stays at the on-screen
       resolution. *)
    let fit = Float.min (cw /. float lw) (ch /. float lh) in
    let dpr = Brr.Window.device_pixel_ratio G.window in
    let bw = int_of_float (Float.round (fit *. float lw *. dpr)) in
    let bh = int_of_float (Float.round (fit *. float lh *. dpr)) in
    if bw <> Canvas.w canvas then Canvas.set_w canvas bw;
    if bh <> Canvas.h canvas then Canvas.set_h canvas bh;
    Jsoo.device_scale := (float bw /. float lw, float bh /. float lh)

  let size ~io:_ =
    let w, h = !Jsoo.logical_size in
    Size.v (float w) (float h)

  let capture_mouse ~io:_ status = Events_js.set_capture status
  let is_mouse_captured ~io:_ = Events_js.is_locked ()

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
  let canvas = io.backend.canvas in
  C.reset_transform ctx;
  C.set_fill_style ctx (C.color (Jstr.of_string "black"));
  C.fill_rect ctx ~x:0.0 ~y:0.0
    ~w:(float (Canvas.w canvas))
    ~h:(float (Canvas.h canvas));
  Gamelle_common.finalize_frame ~io

let run ~canvas state update =
  let open Jsoo in
  Events_js.attach ~canvas;
  let canvas = Canvas.of_el canvas in
  Canvas.set_w canvas (640 * 2);
  Canvas.set_h canvas (480 * 2);
  Jsoo.logical_size := (640 * 2, 480 * 2);

  let ctx = C.get_context canvas in
  let audio = Audio.Context.create () in

  let backend =
    { canvas; ctx; audio; font = Font.default; font_size = Font.default_size }
  in
  let io = make_io backend in
  let clock_ref = ref 0 in

  (* Record when the tab hides and how long it stayed hidden, so the frame that
     resumes measures only the time the game was actually visible. *)
  ignore
    (Ev.listen Ev.visibilitychange
       (fun _ ->
         let t = Performance.now_ms G.performance /. 1000.0 in
         if
           Jstr.equal
             (Document.visibility_state G.document)
             Document.Visibility_state.hidden
         then hidden_at := Some t
         else
           match !hidden_at with
           | Some t0 ->
               hidden_total := !hidden_total +. (t -. t0);
               hidden_at := None
           | None -> ())
       (Document.as_target G.document));

  let rec animate state =
    let _ = G.request_animation_frame (loop state) in
    ()
  and loop state elapsed =
    let open Events_backend in
    let prev_time = !(io.event).time in
    let now = (elapsed /. 1000.0) -. !hidden_total in
    let frame_dt =
      if !started then now -. !prev_now
      else (
        started := true;
        Gamelle_common.target_dt)
    in
    prev_now := now;
    Events_js.new_frame ();
    io_reset_mutable_fields io;
    io.event :=
      {
        !Events_js.current with
        clock = !clock_ref;
        dt = frame_dt;
        time = prev_time +. frame_dt;
      };
    incr clock_ref;
    let state = update ~io state in
    finalize_frame ~io;
    Events_js.current := reset_mouse_delta (reset_wheel !Events_js.current);
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
