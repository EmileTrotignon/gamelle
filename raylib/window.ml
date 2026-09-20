open Gamelle_common
open Geometry

let current_size = ref (0, 0)

(* Apply a resize the game requested this frame (via [Window.set_size]) to the OS
   window. A no-op for an offscreen [Texture] render, which has a fixed size. *)
let apply_requested_size ~io =
  match io.render_target with
  | Gamelle_common.View.Onscreen { requested_size = Some ((w, h) as s) } when s <> !current_size
    ->
      Raylib.set_window_size w h;
      current_size := s
  | _ -> ()

(* An offscreen [Texture] render reports its own fixed size; an onscreen [io]
   reports the real window size. *)
let size ~io =
  match io.render_target with
  | Gamelle_common.View.Texture { width; height } -> Size.v (float width) (float height)
  | Gamelle_common.View.Onscreen _ ->
      let w = Raylib.get_screen_width () in
      let h = Raylib.get_screen_height () in
      Size.v (float w) (float h)

let show_cursor ~io:_ show =
  if show then Raylib.show_cursor () else Raylib.hide_cursor ()

(* Tracked here rather than queried from raylib, because [is_cursor_hidden]
   cannot distinguish a captured cursor from one hidden by [show_cursor]. *)
let captured = ref false

let capture_mouse ~io:_ capture =
  if capture <> !captured then begin
    captured := capture;
    if capture then Raylib.disable_cursor () else Raylib.enable_cursor ()
  end

(* Capture is immediate on raylib, so this is just the requested state. *)
let is_mouse_captured ~io:_ = !captured

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
  apply_requested_size ~io;
  Raylib.clear_background (Raylib.Color.create 0 0 0 255);
  Gamelle_common.finalize_frame ~io
