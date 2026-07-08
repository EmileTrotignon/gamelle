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
  let q1p1 = Vec.(q1 - p1) in
  if Xy.equal_float r_cross_s 0. then
    (* Parallel. If collinear, the intersection may be a whole sub-segment:
       return its middle so that overlapping segments do report a point. *)
    let rr = Vec.norm2 r in
    if rr = 0. then
      (* [p1, p2] is a single point: on [q1, q2]? *)
      let ss = Vec.norm2 s in
      if ss = 0. then if Vec.equal p1 q1 then Some p1 else None
      else
        let u = Vec.(dot (p1 - q1) s) /. ss in
        if Xy.equal_float Vec.(cross (p1 - q1) s) 0. && u >= 0. && u <= 1. then
          Some p1
        else None
    else if not (Xy.equal_float (Vec.cross q1p1 r) 0.) then None
    else
      (* Overlap of [q1, q2] in the parameter space of [p1, p2]. *)
      let t1 = Vec.dot q1p1 r /. rr in
      let t2 = Vec.(dot (q2 - p1) r) /. rr in
      let lo = Float.max (Float.min t1 t2) 0. in
      let hi = Float.min (Float.max t1 t2) 1. in
      if lo > hi then None else Some Vec.(p1 + (0.5 *. (lo +. hi) * r))
  else
    let t = Vec.(cross q1p1 s /. r_cross_s) in
    let u = Vec.(cross q1p1 r /. r_cross_s) in
    if t >= 0. && t <= 1. && u >= 0. && u <= 1. then
      let inter = Vec.(p1 + (t * r)) in
      Some inter
    else None

(* Same as [intersection], for a ray: [origin] + [t * dir] with unbounded
   [t >= 0]. On a collinear overlap, the nearest overlapping point is
   returned. *)
let ray_intersection origin dir { start = q1; end_ = q2 } =
  let s = Vec.(q2 - q1) in
  let d_cross_s = Vec.cross dir s in
  let oq1 = Vec.(q1 - origin) in
  if Xy.equal_float d_cross_s 0. then
    let dd = Vec.norm2 dir in
    if dd = 0. || not (Xy.equal_float (Vec.cross oq1 dir) 0.) then None
    else
      let t1 = Vec.dot oq1 dir /. dd in
      let t2 = Vec.(dot (q2 - origin) dir) /. dd in
      if Float.max t1 t2 < 0. then None
      else
        let t = Float.max (Float.min t1 t2) 0. in
        Some Vec.(origin + (t * dir))
  else
    let t = Vec.cross oq1 s /. d_cross_s in
    let u = Vec.cross oq1 dir /. d_cross_s in
    if t >= 0. && u >= 0. && u <= 1. then Some Vec.(origin + (t * dir))
    else None

let intersect s s' = Option.is_some (intersection s s')
let equal s s' = Vec.equal s.start s'.start && Vec.equal s.end_ s'.end_

let translate vec { start; end_ } =
  { start = Point.translate start vec; end_ = Point.translate end_ vec }

let map_points f { start; end_ } = { start = f start; end_ = f end_ }
let center { start; end_ } = Vec.(0.5 * (start + end_))

let pp h { start; end_ } =
  Format.fprintf h "Segment.v %a %a" Point.pp start Point.pp end_
