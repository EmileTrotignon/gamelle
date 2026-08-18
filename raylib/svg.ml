open Common
open Gamelle_common

(* raylib has no vector support, so an SVG is rasterised (with nanosvg) into a
   texture and thereafter drawn like a bitmap — which is what gives it the view
   transform (rotation, scale) and clipping for free, through the same
   [draw_texture_pro] + [with_scissor] path bitmaps use.

   To stay crisp under zoom (matching the browser, which re-rasterises the vector
   at display resolution every frame), the texture is rasterised at the on-screen
   resolution the SVG is actually drawn at, and re-rasterised when that grows.
   Re-rastering every frame of a continuous zoom would be wasteful, so the
   texture resolution is quantised to integer multiples of the intrinsic size and
   only ever grown: zooming in past the current level re-rasters once at the next
   level, zooming back out reuses the (higher-res) texture and samples it down.
   The level is capped so a huge zoom degrades to blur rather than allocating an
   unbounded texture.

   [tex_w]/[tex_h] are the current texture's dimensions and [tex_scale] the
   svg-unit→texel factor it was rasterised at; [w]/[h] are the intrinsic logical
   size the texture is drawn back down to.

   Known limitation / future work: this rasterises the *whole* SVG at the zoomed
   resolution, so memory (and the raster cost) grow with intrinsic_size × zoom²,
   which is why there is a [max_tex_dim] cap at all — past it (~25x for a 160px
   SVG) the zoom degrades to blur, whereas the browser stays crisp much further.
   The browser only ever rasterises the *visible* pixels (its canvas is a fixed
   size), so its effective resolution is bounded by the viewport, not the SVG. We
   should do the same eventually: rasterise only the on-screen region of the SVG
   at display resolution. That bounds the work and memory by the viewport instead
   of by intrinsic_size × zoom, removes the need for an arbitrary cap, and matches
   the browser at extreme zoom.

   Possible optimisation, deferred until profiling shows it matters: each step of
   a zoom-in re-rasterises from scratch and the previous level's texture is
   discarded, and two [Svg.t] values loaded from the same source rasterise
   independently. The nanosvg raster could be cached instead — e.g. keep the
   already-rasterised levels rather than discarding them, and/or share a cache
   keyed on (source, level) across [Svg.t] values. That is more machinery than is
   warranted now (a zoom-in only re-rasters a handful of times, once per integer
   level, then reuses the result), so it should only be added if a profile shows
   rasterisation is actually a hotspot. *)

type s = {
  image : Nanosvg.Image_data.t;
  rast : Nanosvg.Rasterizer.t;
  w : int;
  h : int;
  mutable texture : Raylib.Texture.t;
  mutable tex_w : int;
  mutable tex_h : int;
  mutable tex_scale : float;
}

type t = (io, s) Delayed.t

let size t = (t.w, t.h)

(* Largest texture edge we will rasterise; beyond it the zoom is served by
   sampling the capped texture up (blurry, but bounded memory). *)
let max_tex_dim = 4096

(* Rasterise the vector into a fresh RGBA buffer at [scale] svg-units→texels. *)
let rasterize_pixels image rast scale =
  let sw = Nanosvg.Image_data.width image in
  let sh = Nanosvg.Image_data.height image in
  let tex_w = Stdlib.max 1 (int_of_float (Float.round (sw *. scale))) in
  let tex_h = Stdlib.max 1 (int_of_float (Float.round (sh *. scale))) in
  let pixels =
    Bigarray.Array1.create Bigarray.Int8_unsigned Bigarray.c_layout
      (tex_w * tex_h * 4)
  in
  Nanosvg.rasterize rast image ~tx:0. ~ty:0. ~scale ~dst:pixels ~w:tex_w
    ~h:tex_h ();
  (pixels, tex_w, tex_h)

(* Allocate an RGBA8 GPU texture and upload [pixels] into it. *)
let make_texture pixels tex_w tex_h =
  let rimg = Raylib.gen_image_color tex_w tex_h Raylib.Color.blank in
  let texture = Raylib.load_texture_from_image rimg in
  Raylib.unload_image rimg;
  Raylib.update_texture texture Ctypes.(to_voidp (bigarray_start array1 pixels));
  Raylib.set_texture_filter texture Raylib.TextureFilter.Bilinear;
  texture

(* Re-rasterise [t] at [scale], updating the texture in place (reallocating only
   when the size changes). *)
let rasterize_at t scale =
  let pixels, tex_w, tex_h = rasterize_pixels t.image t.rast scale in
  if tex_w = t.tex_w && tex_h = t.tex_h then
    Raylib.update_texture t.texture
      Ctypes.(to_voidp (bigarray_start array1 pixels))
  else begin
    Raylib.unload_texture t.texture;
    t.texture <- make_texture pixels tex_w tex_h;
    t.tex_w <- tex_w;
    t.tex_h <- tex_h
  end;
  t.tex_scale <- scale

(* The largest integer zoom level whose texture fits within [max_tex_dim]. *)
let max_level t =
  let longest = float (Stdlib.max t.w t.h) in
  Stdlib.max 1 (int_of_float (float max_tex_dim /. Float.max 1.0 longest))

(* Ensure the texture is rasterised finely enough for an on-screen scale of
   [scale] (svg-units→screen-pixels): if the drawn size exceeds the current
   texture's resolution, re-rasterise at the next integer level (grow only, so
   zooming back out reuses the higher-res texture). *)
let ensure_scale t scale =
  if scale > t.tex_scale +. 1e-3 then begin
    let level = Stdlib.min (max_level t) (int_of_float (Float.ceil scale)) in
    let level = Stdlib.max 1 level in
    if float level > t.tex_scale +. 1e-3 then rasterize_at t (float level)
  end

let load ~w ~h data =
  Delayed.make @@ fun ~io:_ ->
  let image =
    match Nanosvg.parse data with
    | Some image -> image
    | None -> failwith "gamelle: invalid SVG"
  in
  let rast = Nanosvg.Rasterizer.create () in
  (* Rasterise at intrinsic (1:1) resolution to start; the first zoomed-in draw
     bumps it up. *)
  let pixels, tex_w, tex_h = rasterize_pixels image rast 1.0 in
  let texture = make_texture pixels tex_w tex_h in
  { image; rast; w; h; texture; tex_w; tex_h; tex_scale = 1.0 }

let free ~io t =
  let t = Delayed.force ~io t in
  Raylib.unload_texture t.texture
