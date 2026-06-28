open Gamelle_common
open Geometry
open Jsoo
module C = Brr_canvas.C2d

type io = Jsoo.io

let set_color ~io c =
  let c = get_color ~io c in
  let r, g, b, a = Color.to_srgbi c in
  let color =
    C.color (Jstr.of_string (Printf.sprintf "rgba(%i,%i,%i,%f)" r g b a))
  in
  C.set_fill_style io.backend.ctx color;
  C.set_stroke_style io.backend.ctx color

let transform ~io =
  let { view; _ } = io in
  C.reset_transform io.backend.ctx;
  let dx, dy = Vec.to_tuple view.translate in
  C.translate io.backend.ctx ~x:dx ~y:dy;
  C.rotate io.backend.ctx view.rotate;
  C.scale io.backend.ctx ~sx:view.scale ~sy:view.scale

let draw ~io bmp p =
  transform ~io;
  let x, y = Vec.to_tuple p in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () -> Bitmap.draw ~io ~ctx bmp ~x ~y)

let fill_rect ~io ?color rect =
  transform ~io;
  let x, y = Vec.to_tuple (Box.top_left rect) in
  let w, h = Vec.to_tuple (Box.size rect) in
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () -> C.fill_rect ctx ~x ~y ~w ~h)

let draw_rect ~io ?color rect =
  transform ~io;
  let x, y = Vec.to_tuple (Box.top_left rect) in
  let w, h = Vec.to_tuple (Box.size rect) in
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () -> C.stroke_rect ctx ~x ~y ~w ~h)

let draw_line ~io ?color segment =
  transform ~io;
  let p0, p1 = Segment.to_tuple segment in
  let x0, y0 = Vec.to_tuple p0 in
  let x1, y1 = Vec.to_tuple p1 in
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      let path = C.Path.create () in
      C.Path.move_to path ~x:x0 ~y:y0;
      C.Path.line_to path ~x:x1 ~y:y1;
      C.stroke io.backend.ctx path)

let path_poly pts =
  let path = C.Path.create () in
  List.iter
    (fun pt ->
      let x, y = Vec.to_tuple pt in
      C.Path.line_to path ~x ~y)
    (Polygon.points pts);
  C.Path.close path;
  path

let draw_poly ~io ?color pts =
  transform ~io;
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      let path = path_poly pts in
      C.stroke ctx path)

let fill_poly ~io ?color pts =
  transform ~io;
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      let path = path_poly pts in
      C.fill ctx path)

let tau = 8.0 *. atan 1.0
let pi = 4.0 *. atan 1.0

(* A rounded-rectangle path built from the canvas' native [arc] corners. Each
   [arc] implicitly connects to the previous one with a straight edge, so the
   four quarter-circles and four sides form a single closed path — drawn as a
   true curve by the canvas, with no seams. [radius] is already clamped. *)
let round_rect_path ~radius box =
  let r = radius in
  let xl = Box.x_left box and xr = Box.x_right box in
  let yt = Box.y_top box and yb = Box.y_bottom box in
  let path = C.Path.create () in
  C.Path.arc path ~cx:(xl +. r) ~cy:(yt +. r) ~r ~start:pi ~stop:(1.5 *. pi);
  C.Path.arc path ~cx:(xr -. r) ~cy:(yt +. r) ~r ~start:(1.5 *. pi) ~stop:tau;
  C.Path.arc path ~cx:(xr -. r) ~cy:(yb -. r) ~r ~start:0.0 ~stop:(0.5 *. pi);
  C.Path.arc path ~cx:(xl +. r) ~cy:(yb -. r) ~r ~start:(0.5 *. pi) ~stop:pi;
  C.Path.close path;
  path

let draw_rounded_rect ~io ?color ~radius box =
  transform ~io;
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () -> C.stroke ctx (round_rect_path ~radius box))

let fill_rounded_rect ~io ?color ~radius box =
  transform ~io;
  set_color ~io color;
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () -> C.fill ctx (round_rect_path ~radius box))

let draw_circle ~io ?color circle =
  transform ~io;
  set_color ~io color;
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = Vec.to_tuple center in
  let path = C.Path.create () in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
      C.stroke ctx path)

let fill_circle ~io ?color circle =
  transform ~io;
  set_color ~io color;
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = Vec.to_tuple center in
  let path = C.Path.create () in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      C.Path.arc path ~cx:x ~cy:y ~r:radius ~start:0.0 ~stop:tau;
      C.fill ctx path)

(* Arcs use the canvas' native [arc]: the outline is the bare curve, the fill is
   the circular sector (arc closed back to the center). The view transform is
   applied to the context, so we draw in world coordinates. *)
let draw_arc ~io ?color arc =
  transform ~io;
  set_color ~io color;
  let x, y = Vec.to_tuple (Arc.center arc) in
  let r = Arc.radius arc in
  let start = Arc.start_angle arc and stop = Arc.end_angle arc in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      let path = C.Path.create () in
      C.Path.arc path ~cx:x ~cy:y ~r ~start ~stop;
      C.stroke ctx path)

let fill_arc ~io ?color arc =
  transform ~io;
  set_color ~io color;
  let x, y = Vec.to_tuple (Arc.center arc) in
  let r = Arc.radius arc in
  let start = Arc.start_angle arc and stop = Arc.end_angle arc in
  let ctx = io.backend.ctx in
  Clip.draw_clip ~io ctx (fun () ->
      let path = C.Path.create () in
      C.Path.move_to path ~x ~y;
      C.Path.arc path ~cx:x ~cy:y ~r ~start ~stop;
      C.Path.close path;
      C.fill ctx path)
