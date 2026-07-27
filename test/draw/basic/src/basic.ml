open Gamelle

let w = 1000.
let h = 600.
let cell_w = w /. 4.
let cell_h = 300.

let cell col row =
  Box.v
    (Point.v (float col *. cell_w) (float row *. cell_h))
    (Size.v cell_w cell_h)

let label ~io b txt =
  Text.draw ~io ~color:Color.white ~size:14
    ~at:(Point.v (Box.x_left b +. 4.) (Box.y_top b +. 4.))
    txt

let () =
  run () @@ fun ~io () ->
  if Input.is_pressed ~io `escape then raise Exit;
  let io = View.drawing_box (Box.v Point.zero (Size.v w h)) io in
  Box.fill ~io ~color:Color.(rgb 40 40 40) (Window.box ~io);

  (* (0,0) Segment *)
  let b = cell 0 0 in
  label ~io b "Segment.draw";
  Segment.draw ~io ~color:Color.cyan
    (Segment.v
       (Point.v (Box.x_left b +. 10.) (Box.y_top b +. 30.))
       (Point.v (Box.x_right b -. 10.) (Box.y_bottom b -. 10.)));
  Segment.draw ~io ~color:Color.orange
    (Segment.v
       (Point.v (Box.x_right b -. 10.) (Box.y_top b +. 30.))
       (Point.v (Box.x_left b +. 10.) (Box.y_bottom b -. 10.)));

  (* (1,0) Box draw + fill *)
  let b = cell 1 0 in
  label ~io b "Box.draw / fill";
  let inner =
    Box.v
      (Point.v (Box.x_left b +. 10.) (Box.y_top b +. 25.))
      (Size.v (cell_w -. 20.) (cell_h -. 35.))
  in
  Box.fill ~io ~color:Color.blue inner;
  Box.draw ~io ~color:Color.yellow inner;

  (* (2,0) Circle draw + fill *)
  let b = cell 2 0 in
  label ~io b "Circle.draw / fill";
  let r = Float.min (cell_w /. 4.) (cell_h /. 4.) -. 5. in
  let mx, my = (Box.x_middle b, Box.y_middle b +. 10.) in
  Circle.fill ~io ~color:Color.red (Circle.v (Point.v (mx -. r -. 5.) my) r);
  Circle.draw ~io ~color:Color.lime (Circle.v (Point.v (mx +. r +. 5.) my) r);

  (* (0,1) Polygon draw + fill *)
  let b = cell 0 1 in
  label ~io b "Polygon.draw / fill";
  let c = Box.center b in
  let cx, cy = (Point.x c, Point.y c) in
  let tri =
    Polygon.v
      [
        Point.v cx (cy -. 65.);
        Point.v (cx +. 20.) (cy -. 65.);
        Point.v (cx +. 20.) cy;
        Point.v (cx +. 60.) (cy +. 40.);
        Point.v (cx -. 60.) (cy +. 40.);
      ]
  in
  Polygon.fill ~io ~color:Color.magenta tri;
  Polygon.draw ~io ~color:Color.gold tri;

  (* Below the polygon: a translucent fill of a degenerate polygon, like the
     visibility polygons a raycast produces — duplicate vertices, collinear
     runs along an edge, a zero-width spike, and a near-duplicate (float
     noise) return point. Any triangulation overlap double-blends the alpha
     into visible streaks. *)
  let fx = cx and fy = cy +. 95. in
  let fan =
    Polygon.v
      [
        Point.v (fx -. 100.) (fy -. 45.);
        Point.v (fx -. 20.) (fy -. 45.);
        Point.v (fx +. 40.) (fy -. 45.);
        Point.v (fx +. 40.) (fy -. 45.);
        Point.v (fx +. 100.) (fy -. 45.);
        Point.v (fx +. 100.) fy;
        Point.v (fx +. 30.) fy;
        Point.v (fx +. 20.) (fy -. 20.);
        Point.v (fx +. 29.99999) fy;
        Point.v (fx -. 40.) fy;
        Point.v (fx -. 40.) (fy +. 45.);
        Point.v (fx -. 100.) (fy +. 45.);
      ]
  in
  Polygon.fill ~io ~color:(Color.rgb ~alpha:0.75 0 255 0) fan;

  (* (1,1) Touching boxes — 4 cells sharing exact edges *)
  let b = cell 1 1 in
  label ~io b "Touching boxes";
  let bx = Box.x_left b +. 10. and by = Box.y_top b +. 25. in
  let bw = (cell_w -. 20.) /. 2. and bh = (cell_h -. 35.) /. 2. in
  Box.fill ~io ~color:Color.teal (Box.v (Point.v bx by) (Size.v bw bh));
  Box.fill ~io ~color:Color.coral (Box.v (Point.v (bx +. bw) by) (Size.v bw bh));
  Box.fill ~io ~color:Color.indigo
    (Box.v (Point.v bx (by +. bh)) (Size.v bw bh));
  Box.fill ~io ~color:Color.violet
    (Box.v (Point.v (bx +. bw) (by +. bh)) (Size.v bw bh));

  (* (2,1) Bitmap + text *)
  let b = cell 2 1 in
  label ~io b "Bitmap + text";
  draw ~io Assets.camel ~at:(Point.v (Box.x_left b +. 10.) (Box.y_top b +. 25.));
  Text.draw ~io ~color:Color.crimson ~size:20
    ~at:(Point.v (Box.x_left b +. 10.) (Box.y_bottom b -. 35.))
    "Hello Gamelle!";

  (* (3,0) Polygon fill with a transparent color: any double-blended triangle
     or seam in the backend's polygon filling shows up against the flat
     background. *)
  let b = cell 3 0 in
  label ~io b "Alpha fill";
  let cx, cy = (Box.x_middle b, Box.y_middle b) in
  let poly =
    Polygon.v
      [
        Point.v (cx -. 10.) (cy -. 100.);
        Point.v (cx +. 30.) (cy -. 100.);
        Point.v (cx +. 30.) cy;
        Point.v (cx +. 90.) (cy +. 80.);
        Point.v (cx -. 90.) (cy +. 80.);
      ]
  in
  (* alpha 0.75: the blend over the (40,40,40) background is 201.03 / 10.03,
     far from a rounding tie, so both backends produce the same 8-bit color
     (a 0.5 alpha blends to 147.5, which the backends round differently). *)
  Polygon.fill ~io ~color:(Color.rgb ~alpha:0.75 255 0 0) poly;

  (* Arc + rounded box overlaid on the segment cell for a quick visual check. *)
  let b = cell 0 0 in
  let c = Box.center b in
  let pi = 4.0 *. atan 1.0 in
  Arc.draw ~io ~color:Color.lime
    (Arc.v c 30. ~start:(-0.25 *. pi) ~stop:(1.1 *. pi));
  Arc.fill ~io ~color:Color.gold
    (Arc.v
       (Point.v (Box.x_right b -. 35.) (Box.y_bottom b -. 35.))
       25. ~start:0. ~stop:(1.3 *. pi));
  let rb =
    Box.v
      (Point.v (Box.x_left b +. 10.) (Box.y_bottom b -. 95.))
      (Size.v 130. 80.)
  in
  Box.fill_rounded ~io ~color:Color.blue ~radius:24. rb;
  Box.draw_rounded ~io ~color:Color.white ~radius:24. rb;

  ()
