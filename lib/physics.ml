(* The physics engine itself lives in [gamelle.physics] (backend-free, usable
   in headless programs); this only adds the rendering helpers. *)
open Gamelle_backend
open Draw_geometry
include Gamelle_physics.Physics

let draw ~io ?color t =
  Shape.draw ~io ?color (shape t);
  let pos = center t in
  draw_line ~io ?color (Segment.v pos Vec.(pos + polar 10.0 (rotation t)))

let fill ~io ?color t = Shape.fill ~io ?color (shape t)
