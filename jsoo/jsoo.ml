module Canvas = Brr_canvas.Canvas
module C = Brr_canvas.C2d
open Brr_webaudio

type font = string Lazy.t

type io_backend = {
  canvas : Canvas.t;
  ctx : C.t;
  audio : Audio.Context.t;
  font : font;
  font_size : int;
}

type io = io_backend Gamelle_common.abstract_io

(* The game draws in a logical coordinate system of [logical_size] pixels; the
   canvas backing store is kept at the element's real on-screen resolution
   (CSS size x devicePixelRatio), which differs from the logical size in
   fullscreen or on high-dpi displays. [device_scale] is the resulting
   logical -> physical pixel scale on each axis: rendering applies it so
   shapes are rasterized at the native resolution instead of being upscaled
   from a logical-size bitmap. *)
let logical_size = ref (640 * 2, 480 * 2)
let device_scale = ref (1.0, 1.0)
