open Gamelle

(* Clipping screenshot scene, split out of the basic drawing scene so the clip
   machinery is exercised on its own. The raylib backend clips by compositing
   each draw through a signed-distance shader over the clip polygon's edges; the
   browser uses its native path clip. Both must agree.

   The first three bands are the clip cases that used to live in [basic]: a box
   clip under a rotated view, a convex-polygon clip and a concave (star) clip,
   each with a full battery of overflowing primitives (translucent fills, AA
   lines, a bitmap, text).

   The last bands target high-vertex clip polygons: a raycast "visibility"
   polygon (as oedipus feeds to [View.clip_polygon] for its minimap and main
   view) easily has more than a hundred vertices. *)

let w = 1000.
let h = 1800.
let band_h = 300.
let band row = Box.v (Point.v 0. (float row *. band_h)) (Size.v w band_h)

let label ~io b txt =
  Text.draw ~io ~color:Color.white ~size:16
    ~at:(Point.v (Box.x_left b +. 6.) (Box.y_top b +. 6.))
    txt

let pi = 4.0 *. atan 1.0

(* A many-vertex, roughly circular polygon (a regular [n]-gon with a little
   radial wobble so it reads as a filled outline, not a circle). With [n > 64]
   this is exactly the case the raylib clip shader truncates. *)
let ngon ~center ~radius ~n =
  Polygon.v
    (List.init n (fun i ->
         let a = (float i /. float n *. 2.0 *. pi) -. (pi /. 2.0) in
         let r = radius *. (1.0 +. (0.06 *. cos (float i *. 5.0))) in
         Point.v
           (Point.x center +. (r *. cos a))
           (Point.y center +. (r *. sin a))))

(* A raycast-style "visibility" fan: rays of varying length from a centre, the
   silhouette a player would see. Many vertices, non-convex. *)
let visibility ~center ~n =
  let radii = [| 130.; 80.; 120.; 55.; 105.; 140.; 95.; 65.; 125.; 85. |] in
  Polygon.v
    (List.init n (fun i ->
         let a = float i /. float n *. 2.0 *. pi in
         let r = radii.(i mod Array.length radii) in
         Point.v
           (Point.x center +. (r *. cos a))
           (Point.y center +. (r *. sin a))))

(* The battery of overflowing primitives reused for the polygon-clip bands: a
   translucent full-width box, overlapping translucent circles, AA diagonal
   lines, a bitmap and some text — all straddling the clip boundary. *)
let battery ~io b =
  let cx = Box.x_middle b and cy = Box.y_middle b in
  Box.fill ~io
    ~color:(Color.rgb ~alpha:0.4 0 200 255)
    (Box.v (Point.v 0. (Box.y_top b +. 10.)) (Size.v w (band_h -. 20.)));
  List.iter
    (fun (dx, (r, g, bl)) ->
      Circle.fill ~io
        ~color:(Color.rgb ~alpha:0.6 r g bl)
        (Circle.v (Point.v (cx +. dx) cy) 70.))
    [ (-120., (255, 0, 0)); (0., (0, 255, 0)); (120., (0, 0, 255)) ];
  for i = 0 to 10 do
    let x = 100. +. (float i *. 80.) in
    Segment.draw ~io ~color:Color.white
      (Segment.v
         (Point.v x (Box.y_top b +. 10.))
         (Point.v (x +. 120.) (Box.y_bottom b -. 10.)))
  done;
  draw ~io Assets.camel ~at:(Point.v 60. (cy -. 40.));
  Text.draw ~io ~color:Color.yellow ~size:34 ~at:(Point.v 300. (cy -. 20.))
    "Clipped, overflowing!"

let () =
  run () @@ fun ~io () ->
  if Input.is_pressed ~io `escape then raise Exit;
  let io = View.drawing_box (Box.v Point.zero (Size.v w h)) io in
  Box.fill ~io ~color:Color.(rgb 40 40 40) (Window.box ~io);

  (* Band 0: clip under rotation. The view is rotated around the band centre;
     the filled circle sits strictly inside the (world-space) clip box, so it
     must be fully visible, while the distant box must be clipped away entirely.
     Content stays away from the clip border because the raylib backend clips to
     the bounding box of the rotated clip region (its scissor is axis-aligned),
     unlike the browser's exact clip path. *)
  let b = band 0 in
  label ~io b "Clip + rotate";
  let angle = 0.5 in
  let center = Box.center b in
  let rot_io =
    let c' = Point.rotate_around ~center:Point.zero (-.angle) center in
    let d = Vec.(c' - center) in
    io |> View.rotate angle |> View.translate d
  in
  let clip_box = Box.v_center center (Size.v 220. 220.) in
  let rot_io = View.clip clip_box rot_io in
  Circle.fill ~io:rot_io ~color:Color.turquoise
    (Circle.v (Box.center clip_box) 95.);
  Box.fill ~io:rot_io ~color:Color.red
    (Box.v (Point.v (-1000.) (-1000.)) (Size.v 500. 500.));

  (* Band 1: a convex (hexagon) clip polygon — six edges, more than a box's
     four; every primitive overflows it and the outline is drawn unclipped on
     top so the clip boundary is visible. *)
  let b = band 1 in
  label ~io b "Convex polygon clip (AA + alpha)";
  let hexagon =
    let cx = Box.x_middle b and cy = Box.y_middle b in
    Polygon.v
      (List.init 6 (fun i ->
           let a = (float i /. 6.0 *. 2.0 *. pi) +. 0.35 in
           Point.v (cx +. (330. *. cos a)) (cy +. (120. *. sin a))))
  in
  let cio = View.clip_polygon hexagon io in
  battery ~io:cio b;
  Polygon.draw ~io ~color:Color.white hexagon;

  (* Band 2: a concave clip polygon — a five-pointed star. Half-planes cannot
     describe it, so the raylib backend clips it through an even-odd coverage
     mask; the browser uses its native non-zero path clip. Same battery. *)
  let b = band 2 in
  label ~io b "Concave clip (star)";
  let star =
    let cx = Box.x_middle b and cy = Box.y_middle b in
    Polygon.v
      (List.init 10 (fun i ->
           let r = if i mod 2 = 0 then 140. else 58. in
           let a = (float i /. 10.0 *. 2.0 *. pi) -. (pi /. 2.0) in
           Point.v (cx +. (r *. cos a)) (cy +. (r *. sin a))))
  in
  let sio = View.clip_polygon star io in
  battery ~io:sio b;
  Polygon.draw ~io ~color:Color.white star;

  (* Band 3: high-vertex convex clip — a 100-gon. A full-band red box is drawn
     clipped to it, so the result must be a red disc confined to the polygon.
     The raylib backend truncates the edge list at 64, leaving the polygon open,
     which lets the fill bleed outside the intended boundary. *)
  let b = band 3 in
  label ~io b "100-gon clip (>64 edges)";
  let poly = ngon ~center:(Box.center b) ~radius:130. ~n:100 in
  let cio = View.clip_polygon poly io in
  Box.fill ~io:cio ~color:Color.red b;
  Polygon.draw ~io ~color:Color.white poly;

  (* Band 4: a raycast-style visibility polygon, ~130 vertices, non-convex — the
     shape oedipus actually clips against. A translucent box overflows it; only
     the interior should be lit. *)
  let b = band 4 in
  label ~io b "Visibility clip (~130 edges)";
  let vis = visibility ~center:(Box.center b) ~n:130 in
  let cio = View.clip_polygon vis io in
  Box.fill ~io:cio ~color:(Color.rgb ~alpha:0.85 0 200 255) b;
  Polygon.draw ~io ~color:Color.white vis;

  (* Band 5: a scaled + translated view (like oedipus's minimap), clipped to a
     high-vertex polygon frozen in that view — exercises the clip freeze under a
     non-identity view at high vertex count. *)
  let b = band 5 in
  label ~io b "Scaled minimap clip";
  let mio =
    io
    |> View.translate (Vec.v (Box.x_middle b -. 240.) (Box.y_top b +. 40.))
    |> View.scale 1.6
  in
  let poly = ngon ~center:(Point.v 130. 130.) ~radius:110. ~n:90 in
  let cio = View.clip_polygon poly mio in
  Box.fill ~io:cio ~color:Color.orange (Box.v Point.zero (Size.v 400. 400.));
  Polygon.draw ~io:mio ~color:Color.white poly;

  ()
