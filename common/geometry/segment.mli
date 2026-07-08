type t [@@deriving yojson]

val v : Point.t -> Point.t -> t
val start : t -> Point.t
val end_ : t -> Point.t
val to_tuple : t -> Point.t * Point.t
val vector : t -> Vec.t

(* If the segments are collinear and overlap, the middle of the shared part is
   returned. *)
val intersection : t -> t -> Point.t option

(* [ray_intersection origin dir seg] is the point where the unbounded ray
   [origin + t * dir], [t >= 0], first hits [seg]. On a collinear overlap, the
   nearest overlapping point is returned. *)
val ray_intersection : Point.t -> Vec.t -> t -> Point.t option
val intersect : t -> t -> bool
val equal : t -> t -> bool
val translate : Vec.t -> t -> t
val map_points : (Point.t -> Point.t) -> t -> t
val center : t -> Point.t
val pp : Format.formatter -> t -> unit
