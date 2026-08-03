open Gamelle_common
open Geometry
open Brr_canvas
module C = C2d

(* [io.clip] holds screen-space polygons (in logical/CSS pixels), each frozen
   when its [View.clip] was applied. The canvas stores clip regions in device
   space, so we define the clip paths under the device-scale transform only,
   then put the caller's world transform back before drawing [f]. Successive
   [C.clip] calls intersect, which is exactly the "shrink only" semantics of
   stacked clips. The clip persists across that transform change until the
   matching [restore]. *)
let draw_clip ~io ctx f =
  match io.clip with
  | [] -> f ()
  | polys ->
      let world = C.get_transform ctx in
      C.save ctx;
      C.reset_transform ctx;
      let dsx, dsy = !Jsoo.device_scale in
      C.scale ctx ~sx:dsx ~sy:dsy;
      List.iter
        begin fun poly ->
          let path = C.Path.create () in
          poly |> Polygon.points
          |> List.iter begin fun p ->
              let x, y = Vec.to_tuple p in
              C.Path.line_to path ~x ~y
            end;
          C.Path.close path;
          C.clip ctx path
        end
        polys;
      C.set_transform ctx world;
      let r = f () in
      C.restore ctx;
      r
