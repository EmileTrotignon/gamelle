type t = { center : Point.t; radius : float } [@@deriving yojson]

let v center radius =
  if radius < 0.0 then invalid_arg "Circle.v: negative radius";
  { center; radius }

let center { center; _ } = center
let radius { radius; _ } = radius

let translate vec { center; radius } =
  { center = Point.translate center vec; radius }

let map_center f { center; radius } = { center = f center; radius }
let area { radius; _ } = Float.pi *. radius *. radius
let square f = f *. f

let intersections { center = c0; radius = r0 } { center = c1; radius = r1 } =
  let dist2 = Vec.(norm2 (c1 - c0)) in
  if dist2 >= square (r0 +. r1) || dist2 <= square (r0 -. r1) then []
  else
    let d = sqrt dist2 in
    let l = (square r0 -. square r1 +. dist2) /. (2.0 *. d) in
    let h = sqrt (square r0 -. square l) in
    let x1, y1 = (c0.x, c0.y) in
    let x2, y2 = (c1.x, c1.y) in
    let ld = l /. d in
    let hd = h /. d in
    let base_x = (ld *. (x2 -. x1)) +. x1 in
    let base_y = (ld *. (y2 -. y1)) +. y1 in
    if hd = 0.0 then [ Point.v base_x base_y ]
    else
      let px = hd *. (y2 -. y1) in
      let py = hd *. (x2 -. x1) in
      [
        Point.v (base_x +. px) (base_y -. py);
        Point.v (base_x -. px) (base_y +. py);
      ]

let intersect { center = c0; radius = r0 } { center = c1; radius = r1 } =
  let dist2 = Vec.(norm2 (c1 - c0)) in
  let r2 = square (r0 +. r1) in
  dist2 < r2

let mem pt { center; radius } =
  let dist2 = Vec.(norm2 (center - pt)) in
  dist2 <= radius *. radius

let pp h { center; radius } =
  Format.fprintf h "Circle.v %a %f" Point.pp center radius
