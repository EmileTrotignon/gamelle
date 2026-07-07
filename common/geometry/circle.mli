type t [@@deriving yojson]

val v : Point.t -> float -> t
val center : t -> Point.t
val radius : t -> float
val translate : Vec.t -> t -> t
val map_center : (Point.t -> Point.t) -> t -> t
val area : t -> float
val intersections : t -> t -> Point.t list
val intersect : t -> t -> bool
val mem : Point.t -> t -> bool
val pp : Format.formatter -> t -> unit
