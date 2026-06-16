open Gamelle_common
open Geometry

let current_size = ref (0, 0)

let set_size ~io =
  let w, h = !(io.window_size) in
  if (w, h) <> (0, 0) && (w, h) <> !current_size then begin
    Raylib.set_window_size w h;
    current_size := (w, h)
  end

let size ~io:_ =
  let w = Raylib.get_screen_width () in
  let h = Raylib.get_screen_height () in
  Size.v (float w) (float h)

let show_cursor ~io:_ show =
  if show then Raylib.show_cursor () else Raylib.hide_cursor ()

let is_fullscreen () =
  Raylib.is_window_state Raylib.ConfigFlags.borderless_windowed_mode

let get_fullscreen ~io:_ = is_fullscreen ()
let windowed_size = ref (640, 640)

let set_fullscreen ~io:_ fullscreen =
  if fullscreen <> is_fullscreen () then
    begin if fullscreen then begin
      windowed_size := (Raylib.get_screen_width (), Raylib.get_screen_height ());
      let m = Raylib.get_current_monitor () in
      Raylib.set_window_size
        (Raylib.get_monitor_width m)
        (Raylib.get_monitor_height m);
      Raylib.set_window_state Raylib.ConfigFlags.borderless_windowed_mode
    end
    else begin
      Raylib.clear_window_state Raylib.ConfigFlags.borderless_windowed_mode;
      let w, h = !windowed_size in
      Raylib.set_window_size w h
    end
    end

let finalize_frame ~io =
  set_size ~io;
  Raylib.clear_background (Raylib.Color.create 0 0 0 255);
  Gamelle_common.finalize_frame ~io
