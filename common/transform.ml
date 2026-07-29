open Geometry
open Xy

type t = { scale : float; translate : Vec.t; rotate : float }

let default = { scale = 1.0; translate = Vec.zero; rotate = 0.0 }
let ( *^ ) f (x, y) = (f *. x, f *. y)

let translate dxy t =
  let x, y = (dxy.x, dxy.y) in
  let c, s = (t.scale *. cos t.rotate, t.scale *. sin t.rotate) in
  let dxy = Vec.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  { t with translate = Vec.(t.translate + dxy) }

let scale factor t = { t with scale = factor *. t.scale }
let rotate angle t = { t with rotate = angle +. t.rotate }

let project { scale; translate = tr; rotate } p =
  let x, y = (p.x, p.y) in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  let p = Point.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y)) in
  Point.(p + tr)

let inv_project { scale; translate = tr; rotate } p =
  let rotate = -.rotate in
  let scale = 1.0 /. scale in
  let p = Point.(p - tr) in
  let x, y = (p.x, p.y) in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  Point.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y))

(* Inverse-project a vector (e.g. a mouse delta): like [inv_project] but without
   the translation, since a displacement is unaffected by the view's origin. *)
let inv_project_vector { scale; translate = _; rotate } v =
  let rotate = -.rotate in
  let scale = 1.0 /. scale in
  let x, y = (v.x, v.y) in
  let c, s = (scale *. cos rotate, scale *. sin rotate) in
  Vec.v ((c *. x) -. (s *. y)) ((s *. x) +. (c *. y))

(* The screen-space bounding box of the projected box: under rotation the
   projected corners are no longer axis-aligned, so this is the smallest
   axis-aligned box containing all four of them. *)
let project_box t box =
  Polygon.bounding_box (Polygon.v (List.map (project t) (Box.corners box)))

(* The polygon of [poly]'s points projected through [t]. Used to freeze a clip
   region into screen space at the moment it is applied: a box projected through
   a rotated view is a (convex) parallelogram, which a box could not represent. *)
let project_polygon t poly = Polygon.map_points (project t) poly
