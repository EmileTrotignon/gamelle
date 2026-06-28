type t = { start : Point.t; end_ : Point.t } [@@deriving yojson]

let v start end_ =
  (* This way, polymorphic equality works on segments *)
  let start = min start end_ and end_ = max start end_ in
  { start; end_ }

let start { start; _ } = start
let end_ { end_; _ } = end_
let to_tuple { start; end_ } = (start, end_)
let vector { start; end_ } = Vec.(end_ - start)

let intersection { start = p1; end_ = p2 } { start = q1; end_ = q2 } =
  (* https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect *)
  let r = Vec.(p2 - p1) and s = Vec.(q2 - q1) in
  let r_cross_s = Vec.cross r s in
  if Xy.equal_float r_cross_s 0. then
    (* colinear case. We do not check for intersection in that case *)
    None
  else
    let t = Vec.(cross (q1 - p1) s /. r_cross_s) in
    let u = Vec.(cross (q1 - p1) r /. r_cross_s) in
    if t >= 0. && t <= 1. && u >= 0. && u <= 1. then
      let inter = Vec.(p1 + (t * r)) in
      Some inter
    else None

let intersect s s' = Option.is_some (intersection s s')
let equal s s' = Vec.equal s.start s.end_ && Vec.equal s'.start s'.end_

let translate vec { start; end_ } =
  { start = Point.translate start vec; end_ = Point.translate end_ vec }

let map_points f { start; end_ } = { start = f start; end_ = f end_ }
let center { start; end_ } = Vec.(0.5 * (start + end_))

let pp h { start; end_ } =
  Format.fprintf h "Segment.v %a %a" Point.pp start Point.pp end_
