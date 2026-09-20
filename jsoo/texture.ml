open Gamelle_common
open Geometry
module Canvas = Brr_canvas.Canvas
module C = Brr_canvas.C2d

(* The page's audio context, captured when [run] starts. A texture render builds
   its own backend [io] with no outer [io] to copy, but the backend record still
   requires an audio context (never used — a texture render draws, it doesn't play
   sound). *)
let audio_ctx : Brr_webaudio.Audio.Context.t option ref = ref None

type t = { canvas : Canvas.t; ctx : C.t; w : int; h : int }

(* Allocate a fresh offscreen canvas of logical size [w]x[h]. Its backing store is
   sized to the current device scale so the texture is rasterised at on-screen
   resolution: [Draw.transform] applies [device_scale] from the shared global, so
   logical coordinates still map 1:1. A new canvas starts transparent.

   The canvas is a plain JS object, so it is reclaimed by the JS garbage collector
   once the texture is unreachable — nothing to free explicitly. *)
let create ~w ~h =
  let dsx, dsy = !Jsoo.device_scale in
  let bw = max 1 (int_of_float (Float.round (float w *. dsx))) in
  let bh = max 1 (int_of_float (Float.round (float h *. dsy))) in
  let canvas = Canvas.create ~w:bw ~h:bh [] in
  let ctx = C.get_context canvas in
  { canvas; ctx; w; h }

let backend_of t =
  let audio =
    match !audio_ctx with
    | Some a -> a
    | None -> Brr_webaudio.Audio.Context.create ()
  in
  {
    Jsoo.canvas = t.canvas;
    ctx = t.ctx;
    audio;
    font = Font_.default;
    font_size = Font_.default_size;
  }

(* Reset the buffer to fully transparent. The transform is reset first so the
   clear covers the whole backing store regardless of any prior transform. *)
let wipe t =
  C.reset_transform t.ctx;
  C.clear_rect t.ctx ~x:0. ~y:0.
    ~w:(float (Canvas.w t.canvas))
    ~h:(float (Canvas.h t.canvas))

(* Render [draw_fn] onto [t], over whatever it already holds (the canvas is never
   cleared, so successive calls accumulate). [draw_fn] gets its own [io] targeting
   the offscreen canvas with a default view, no clip and its own deferred-draw
   list; its draws are flushed with [finalize_frame], resolving their internal
   z-ordering as they are composited. *)
let draw_on t draw_fn =
  let tio =
    {
      (Gamelle_common.make_io (backend_of t)) with
      render_target =
        Gamelle_common.View.Texture { width = t.w; height = t.h };
    }
  in
  draw_fn ~io:tio;
  Gamelle_common.finalize_frame ~io:tio

(* Draw the texture as the same transformed image as a bitmap: the offscreen
   canvas is blitted at its logical size, honouring the view transform and clip. *)
let draw ~io t p =
  Draw.transform ~io;
  let x, y = Vec.to_tuple p in
  let ctx = io.backend.Jsoo.ctx in
  let img = C.image_src_of_el (Canvas.to_el t.canvas) in
  Clip.draw_clip ~io ctx (fun () ->
      C.draw_image_in_rect ctx img ~x ~y ~w:(float t.w) ~h:(float t.h))
