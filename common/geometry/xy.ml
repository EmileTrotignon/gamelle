type t = { x : float; y : float } [@@deriving yojson]

let zero = { x = 0.0; y = 0.0 }
let v x y = { x; y }
let polar norm angle = { x = norm *. cos angle; y = norm *. sin angle }
let epsilon = 0.0001
let equal_float a b = abs_float (a -. b) < epsilon
let equal a b = equal_float a.x b.x && equal_float a.y b.y

let compare a b =
  match abs_float (a.x -. b.x) with
  | e when abs_float e < epsilon -> (
      match abs_float (a.y -. b.y) with
      | e when abs_float e < epsilon -> 0
      | e when e > 0.0 -> 1
      | _ -> -1)
  | e when e > 0.0 -> 1
  | _ -> -1

let ( + ) a b = { x = a.x +. b.x; y = a.y +. b.y }
let ( - ) a b = { x = a.x -. b.x; y = a.y -. b.y }
let ( * ) f t = { x = f *. t.x; y = f *. t.y }
let translate p vec = p + vec
let lerp t a b = a + (t * (b - a))
let norm2 { x; y } = (x *. x) +. (y *. y)
let norm t = sqrt (norm2 t)
let unit t = 1.0 /. norm t * t
let ortho { x; y } = { x = -.y; y = x }
let dot a b = (a.x *. b.x) +. (a.y *. b.y)
let cross a b = (a.x *. b.y) -. (a.y *. b.x)

let rotate_around ~angle:(cos, sin) ~center pt =
  let at = pt - center in
  let x, y = (at.x, at.y) in
  let pt = v ((x *. cos) -. (y *. sin)) ((y *. cos) +. (x *. sin)) in
  pt + center

let to_tuple t = (t.x, t.y)
let x t = t.x
let y t = t.y
let pp h { x; y } = Format.fprintf h "{ x = %f; y = %f }" x y
