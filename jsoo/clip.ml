open Gamelle_common
open Geometry
open Brr_canvas
module C = C2d

(* The clip region is a screen-space polygon (in logical/CSS pixels), frozen
   when [View.clip] was applied. The canvas stores clip regions in device space,
   so we define the clip path under the device-scale transform only, then put
   the caller's world transform back before drawing [f]. The clip persists
   across that transform change until the matching [restore]. *)
let draw_clip ~io ctx f =
  match io.clip with
  | None -> f ()
  | Some poly ->
      let world = C.get_transform ctx in
      C.save ctx;
      C.reset_transform ctx;
      let dsx, dsy = !Jsoo.device_scale in
      C.scale ctx ~sx:dsx ~sy:dsy;
      let path = C.Path.create () in
      poly |> Polygon.points
      |> List.iter begin fun p ->
          let x, y = Vec.to_tuple p in
          C.Path.line_to path ~x ~y
        end;
      C.Path.close path;
      C.clip ctx path;
      C.set_transform ctx world;
      let r = f () in
      C.restore ctx;
      r
