include Common
module Geometry = Gamelle_common.Geometry
module Bitmap = Bitmap
module Font = Font_
module Sound = Sound
module Transform = Gamelle_common.Transform
module Text = Text
include Draw
module Tsdl_image = Tsdl_image.Image
module Ttf = Tsdl_ttf.Ttf
module Window = Window

let desired_time = 1.0 /. 60.0

let await_event () =
  Sdl.flush_events min_int max_int;
  let run = State.get_current_run () in
  while
    (not (Sdl.wait_event_timeout None 20)) && run == State.get_current_run ()
  do
    ()
  done

let run () =
  let& () = Sdl.init Sdl.Init.(video + audio) in
  Sdl.start_text_input ();
  let _ = Tsdl_image.init Tsdl_image.Init.(jpg + png) in
  let& _ = Tsdl_mixer.Mixer.init Tsdl_mixer.Mixer.Init.(ogg + mp3) in
  let& _ = Ttf.init () in

  let _ok : bool =
    Tsdl.Sdl.set_hint Tsdl.Sdl.Hint.render_scale_quality "nearest"
  in
  let _ok : bool = Tsdl.Sdl.set_hint Tsdl.Sdl.Hint.render_vsync "1" in

  let& () =
    Tsdl_mixer.Mixer.open_audio 44_100 Tsdl_mixer.Mixer.default_format 2 2048
  in

  ignore @@ Tsdl_mixer.Mixer.allocate_channels Gamelle_common.max_sounds;

  let& window = Sdl.create_window "Test" ~w:640 ~h:640 Sdl.Window.(windowed) in
  let& renderer =
    Sdl.create_renderer ~flags:Sdl.Renderer.(accelerated + presentvsync) window
  in

  let backend =
    { window; renderer; font = Font.default; font_size = Font.default_size }
  in

  let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
  let start_time = t0 in
  let now_prev = ref t0 in
  let now = ref t0 in

  let open Gamelle_common in
  let events = ref Events_backend.default in

  let io = make_io backend in

  let rec loop () : unit =
    let t0 = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let t0 = t0 -. start_time in
    now_prev := !now;
    now := t0;

    let was_replayed =
      mutex_protect State.lock @@ fun () -> Replay.replay ~events ~io
    in

    Sdl.pump_events ();
    let event = Events_sdl.update ~clock:!Replay.clock !(io.event) in
    if Events_backend.is_pressed event `quit then raise Exit;

    let has_focus = Window.has_focus window in

    if
      was_replayed = `not_replayed
      && (not (State.crashed ()))
      && ((has_focus && event.mouse_y > !Gamelle_common.ui_replay_height)
         || !Replay.clock = 0)
    then (
      io_reset_mutable_fields io;
      io.event := event;
      Replay.add event;
      State.update_frame ~io);

    Replay.draw_progress ~io ();

    Window.finalize_frame ~io;
    Sdl.render_present renderer;

    if State.crashed () || ((not has_focus) && was_replayed <> `replay_progress)
    then await_event ();

    let now = Int32.to_float (Sdl.get_ticks ()) /. 1000.0 in
    let frame_elapsed = now -. t0 in
    let wait_time =
      Int32.of_float (max 0.001 (1000.0 *. (desired_time -. frame_elapsed)))
    in
    Sdl.delay wait_time;

    loop ()
  in
  (try loop () with Exit -> ());
  State.clean ();
  Tsdl_mixer.Mixer.close_audio ();
  Tsdl_mixer.Mixer.quit ();
  Tsdl_image.quit ();
  Sdl.destroy_renderer renderer;
  Sdl.destroy_window window

let run state update = State.run state update ~start:run ~reload:Replay.reload

module Net = struct
  (* TODO: networking is not implemented on the SDL backend. *)
  type t = unit

  let todo () = failwith "Net is not implemented on the SDL backend (TODO)"
  let connect _ = todo ()
  let send _ _ = todo ()
  let poll _ = todo ()
  let close _ = todo ()
end
