open Common
open Gamelle_common
open Geometry
module Gfx = Tsdl_gfx.Gfx
module Delayed = Gamelle_common.Delayed

let get_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  (r, g, b, a)

let tau = 8.0 *. atan 1.0
let point_zero = Sdl.Point.create ~x:0 ~y:0

let project ~io p =
  let x, y = Vec.to_tuple (Transform.project io.view p) in
  (int x, int y)

let draw_clip ~io renderer f =
  let clip = io.clip in
  let* () =
    Option.map
      (fun clip ->
        let clip = Transform.project_box io.view clip in
        let clip_rect =
          Sdl.Rect.create
            ~x:((int @@ Box.x_left clip) + 1)
            ~y:((int @@ Box.y_top clip) + 1)
            ~w:((int @@ Box.width clip) - 1)
            ~h:((int @@ Box.height clip) - 1)
        in
        Sdl.render_set_clip_rect renderer (Some clip_rect))
      clip
    |> function
    | None -> Ok ()
    | Some v -> v
  in
  let r = f () in
  let* () =
    if Option.is_some clip then
      let win = Window.box ~io in
      let clip_rect =
        Sdl.Rect.create
          ~x:(int @@ Box.x_left win)
          ~y:(int @@ Box.y_top win)
          ~w:(int @@ Box.width win)
          ~h:(int @@ Box.height win)
      in
      Sdl.render_set_clip_rect renderer (Some clip_rect)
    else Ok ()
  in
  r

let draw ~io bmp p =
  let bmp = Delayed.force ~io bmp in
  let scale = io.view.scale in
  let x, y = project ~io p in
  let w, h = Bitmap.size bmp in
  let src = Sdl.Rect.create ~x:0 ~y:0 ~w ~h in

  let dst =
    Sdl.Rect.create ~x ~y
      ~w:(int (scale *. float w))
      ~h:(int (scale *. float h))
  in

  let open Bitmap in
  let angle = io.view.rotate *. 360.0 /. tau in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Sdl.render_copy_ex ~src ~dst renderer bmp.bmp angle (Some point_zero)
          Sdl.Flip.none)
  in
  ()

let draw_line ~io ?color segment =
  let p, p' = Segment.to_tuple segment in
  let x1, y1 = project ~io p in
  let x2, y2 = project ~io p' in
  let r, g, b, a = get_color ~io color in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.aaline_rgba renderer ~x1 ~y1 ~x2 ~y2 ~r ~g ~b ~a)
  in
  ()

let draw_rect ~io ?color rect =
  draw_line ~io ?color (Box.top rect);
  draw_line ~io ?color (Box.bottom rect);
  draw_line ~io ?color (Box.left rect);
  draw_line ~io ?color (Box.right rect)

let draw_poly ~io ?color poly =
  let arr = Polygon.points poly in
  let arr = List.map (project ~io) arr in
  let r, g, b, a = get_color ~io color in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.aapolygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_poly ~io ?color poly =
  let arr = Polygon.points poly in
  let arr = List.map (project ~io) arr in
  let r, g, b, a = get_color ~io color in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.filled_polygon_rgba renderer ~ps:arr ~r ~g ~b ~a)
  in
  ()

let fill_rect ~io ?color rect =
  let p0 = Box.top_left rect
  and p1 = Box.top_right rect
  and p2 = Box.bottom_right rect
  and p3 = Box.bottom_left rect in
  let pts = Polygon.v [ p0; p1; p2; p3 ] in
  fill_poly ~io ?color pts

(* Rounded rectangles are tessellated: the four corners become quarter arcs of
   [radius] (already clamped by the caller), strung into a single closed
   polygon. *)
let rounded_rect_points ~radius box =
  let r = radius in
  let xl = Box.x_left box and xr = Box.x_right box in
  let yt = Box.y_top box and yb = Box.y_bottom box in
  let pi = 4.0 *. atan 1.0 in
  let corner cx cy start =
    Arc.to_points (Arc.v (Point.v cx cy) r ~start ~stop:(start +. (pi /. 2.0)))
  in
  Polygon.v
    (List.concat
       [
         corner (xl +. r) (yt +. r) pi;
         corner (xr -. r) (yt +. r) (1.5 *. pi);
         corner (xr -. r) (yb -. r) 0.0;
         corner (xl +. r) (yb -. r) (0.5 *. pi);
       ])

let draw_rounded_rect ~io ?color ~radius box =
  draw_poly ~io ?color (rounded_rect_points ~radius box)

let fill_rounded_rect ~io ?color ~radius box =
  fill_poly ~io ?color (rounded_rect_points ~radius box)

let draw_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = project ~io center in
  let radius = int (io.view.scale *. radius) in
  let r, g, b, a = get_color ~io color in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        Gfx.aacircle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()

(* Arcs are tessellated into segments: the outline as a polyline, the fill as
   the circular sector (center + arc points) handed to [fill_poly]. *)
let draw_arc ~io ?color arc =
  let rec go = function
    | p0 :: (p1 :: _ as rest) ->
        draw_line ~io ?color (Segment.v p0 p1);
        go rest
    | _ -> ()
  in
  go (Arc.to_points arc)

let fill_arc ~io ?color arc =
  let pts = Arc.center arc :: Arc.to_points arc in
  fill_poly ~io ?color (Polygon.v pts)

let fill_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let x, y = project ~io center in
  let radius = int (io.view.scale *. radius) in
  let r, g, b, a = get_color ~io color in
  let& () =
    let renderer = io.backend.renderer in
    draw_clip ~io renderer (fun () ->
        let* () =
          Gfx.filled_circle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a
        in
        Gfx.aacircle_rgba renderer ~x ~y ~rad:radius ~r ~g ~b ~a)
  in
  ()
