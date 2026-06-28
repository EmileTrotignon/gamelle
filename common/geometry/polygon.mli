type t [@@deriving yojson]

val v : Point.t list -> t
val points : t -> Point.t list
val center : t -> Point.t
val signed_area : t -> float
val segments : t -> Segment.t list
val translate : Vec.t -> t -> t
val map_points : (Point.t -> Point.t) -> t -> t
val bounding_box : t -> Box.t
val pp : Format.formatter -> t -> unit
