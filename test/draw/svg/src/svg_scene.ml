open Gamelle

(* SVG screenshot scene: draw the same vector logo five ways — plain, under a
   rotated view, under a clip, zoomed in and zoomed out — so the raylib
   (nanosvg-rasterised texture) and browser (native SVG) renderings can be
   compared, and so the view transform, clipping and zoom paths are exercised on
   an SVG the same way the [basic] and [clip] scenes exercise them on bitmaps and
   shapes. The zoom cells in particular check the SVG stays crisp when scaled up
   (and minifies cleanly when scaled down), matching the browser's
   display-resolution rasterisation. *)

let cell_w = 300.
let cell_h = 300.
let w = cell_w *. 5.
let h = cell_h
let cell col = Box.v (Point.v (float col *. cell_w) 0.) (Size.v cell_w cell_h)
let pi = 4.0 *. atan 1.0

let label ~io b txt =
  Text.draw ~io ~color:Color.white ~size:16
    ~at:(Point.v (Box.x_left b +. 6.) (Box.y_top b +. 6.))
    txt

(* Draw the logo centred in [b] at its intrinsic size. *)
let draw_logo ~io b =
  let sw = float (Svg.width Assets.logo)
  and sh = float (Svg.height Assets.logo) in
  let at =
    Point.v (Box.x_middle b -. (sw /. 2.0)) (Box.y_middle b -. (sh /. 2.0))
  in
  Svg.draw ~io ~at Assets.logo

(* Draw the logo in cell [col], with the view scaled by [factor] about the cell
   centre (a real zoom in or out), clipped to the cell so the result stays
   contained. *)
let zoom_cell ~io col ~factor txt =
  let b = cell col in
  label ~io b txt;
  let c = Box.center b in
  let k = (1.0 -. factor) /. factor in
  let zio =
    io |> View.clip b |> View.scale factor
    |> View.translate (Vec.v (k *. Point.x c) (k *. Point.y c))
  in
  draw_logo ~io:zio b

let () =
  run () @@ fun ~io () ->
  if Input.is_pressed ~io `escape then raise Exit;
  let io = View.drawing_box (Box.v Point.zero (Size.v w h)) io in
  Box.fill ~io ~color:Color.(rgb 40 40 40) (Window.box ~io);

  (* Cell 0: drawn plainly at intrinsic size. *)
  let b = cell 0 in
  label ~io b "Svg.draw";
  draw_logo ~io b;

  (* Cell 1: the view is rotated about the cell centre, so the logo tilts in
     place — the SVG must follow the view rotation like a bitmap. *)
  let b = cell 1 in
  label ~io b "Svg + rotate";
  let angle = 0.4 in
  let center = Box.center b in
  let rot_io =
    let c' = Point.rotate_around ~center:Point.zero (-.angle) center in
    let d = Vec.(c' - center) in
    io |> View.rotate angle |> View.translate d
  in
  draw_logo ~io:rot_io b;

  (* Cell 2: clipped to a concave star polygon, so the SVG only shows through
     the star's points — a non-convex clip that a box could not express. The
     star outline is drawn (unclipped) in pink on top so the intended clip
     region is visible against what actually survives. *)
  let b = cell 2 in
  label ~io b "Svg + clip";
  let star =
    let cx = Box.x_middle b and cy = Box.y_middle b in
    let outer = 100.0 and inner = 42.0 in
    Polygon.v
      (List.init 10 (fun i ->
           let r = if i mod 2 = 0 then outer else inner in
           let a = (float i /. 10.0 *. 2.0 *. pi) -. (pi /. 2.0) in
           Point.v (cx +. (r *. cos a)) (cy +. (r *. sin a))))
  in
  let cio = View.clip_polygon star io in
  draw_logo ~io:cio b;
  Polygon.draw ~io ~color:Color.(rgb 255 105 180) star;

  (* Cell 3: zoomed in 6x. This is the case that must stay crisp: the browser
     re-rasterises the vector at display resolution, so any raylib blur here
     (from sampling too coarse a raster up) shows as a large diff against it. *)
  zoom_cell ~io 3 ~factor:6.0 "Svg + zoom 6x";

  (* Cell 4: zoomed out 6x, so the logo is drawn much smaller than intrinsic —
     the minification counterpart of cell 3, checking the downscaled SVG looks as
     clean as the browser's. *)
  zoom_cell ~io 4 ~factor:(1.0 /. 6.0) "Svg + zoom /6"
