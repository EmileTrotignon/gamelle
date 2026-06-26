include Common
module Geometry = Gamelle_common.Geometry
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
module Text = Text
include Draw
module Window = Window

(* When [GAMELLE_SCREENSHOT] is set, the run loop renders a few frames, writes a
   PNG to that path and exits. Used by the headless raylib screenshot tests; the
   audio device is skipped so it works under xvfb. *)
let screenshot_path = Sys.getenv_opt "GAMELLE_SCREENSHOT"

let screenshot_frame =
  match Sys.getenv_opt "GAMELLE_SCREENSHOT_FRAME" with
  | Some n -> ( try int_of_string n with _ -> 3)
  | None -> 3

let run state update =
  Raylib.set_config_flags Raylib.ConfigFlags.(msaa_4x_hint + window_highdpi);
  Raylib.set_trace_log_level Raylib.TraceLogLevel.Warning;
  Raylib.init_window 640 640 "Gamelle";
  (* Raylib.begin_blend_mode Raylib.BlendMode.Alpha_premultiply; *)
  Raylib.set_target_fps 60;
  if screenshot_path = None then Raylib.init_audio_device ();

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
    (match screenshot_path with
    | Some path when !clock_ref >= screenshot_frame ->
        (* Flush the batched draw calls to the framebuffer before reading it
           back, then capture before [end_drawing] swaps buffers. *)
        Raylib.Rlgl.draw_render_batch_active ();
        Raylib.take_screenshot path;
        running := false
    | _ -> if screenshot_path = None then Sound.update_current_music ());
    Raylib.end_drawing ()
  done;
  if screenshot_path = None then begin
    Sound.cleanup ();
    Raylib.close_audio_device ()
  end;
  Raylib.close_window ()
