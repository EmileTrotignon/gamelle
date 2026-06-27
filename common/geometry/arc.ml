type t = {
  center : Point.t;
  radius : float;
  start_angle : float;
  end_angle : float;
}

let v center radius ~start ~stop =
  if radius < 0.0 then invalid_arg "Arc.v: negative radius";
  { center; radius; start_angle = start; end_angle = stop }

let of_circle ~start ~stop c = v (Circle.center c) (Circle.radius c) ~start ~stop

let center { center; _ } = center
let radius { radius; _ } = radius
let start_angle { start_angle; _ } = start_angle
let end_angle { end_angle; _ } = end_angle

(* The signed angular span of the arc, in radians. *)
let angle { start_angle; end_angle; _ } = end_angle -. start_angle

let translate vec t = { t with center = Point.translate t.center vec }
let map_center f t = { t with center = f t.center }

let point_at { center; radius; _ } angle =
  Point.translate center (Vec.polar radius angle)

let start_point t = point_at t t.start_angle
let end_point t = point_at t t.end_angle

(* Tessellate the arc into [n + 1] points evenly spaced along its span. When
   [n] is omitted, the number of segments is chosen from the arc length so the
   approximation stays smooth. *)
let to_points ?n t =
  let span = angle t in
  let n =
    match n with
    | Some n -> max 1 n
    | None -> max 1 (int_of_float (Float.abs span *. t.radius /. 4.0))
  in
  List.init (n + 1) (fun i ->
      let a = t.start_angle +. (span *. float i /. float n) in
      point_at t a)

let pp h { center; radius; start_angle; end_angle } =
  Format.fprintf h "Arc.v %a %f ~start:%f ~stop:%f" Point.pp center radius
    start_angle end_angle
