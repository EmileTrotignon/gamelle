open Gamelle

let w = 800.
let h = 600.
let cell_w = w /. 3.
let cell_h = h /. 2.

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
        Point.v (cx +. 60.) (cy +. 40.);
        Point.v (cx -. 60.) (cy +. 40.);
      ]
  in
  Polygon.fill ~io ~color:Color.magenta tri;
  Polygon.draw ~io ~color:Color.gold tri;

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

  ()
