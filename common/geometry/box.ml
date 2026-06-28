open Xy

type t = { tl : Point.t; size : Size.t } [@@deriving yojson]

let v tl size = { tl; size }

let v_center center size =
  let half = Size.(0.5 * size) in
  { tl = Point.(center - half); size }

let center t = Point.(t.tl + (0.5 * t.size))
let size t = t.size
let zero = { tl = Point.zero; size = Size.zero }
let top_left t = t.tl
let top_right t = Point.(t.tl + v (Size.width t.size) 0.0)
let bottom_left t = Point.(t.tl + v 0.0 (Size.height t.size))
let bottom_right t = Point.(t.tl + t.size)
let top box = Segment.v (top_left box) (top_right box)
let left box = Segment.v (top_left box) (bottom_left box)
let right box = Segment.v (top_right box) (bottom_right box)
let bottom box = Segment.v (bottom_left box) (bottom_right box)
let height box = Size.height box.size
let width box = Size.width box.size
let translate vec box = { tl = Point.translate vec box.tl; size = box.size }
let v_corners p1 p2 = v p1 Vec.(p2 - p1)
let x_left t = t.tl.x
let x_right t = t.tl.x +. t.size.x
let x_middle t = t.tl.x +. (t.size.x /. 2.0)
let y_top t = t.tl.y
let y_bottom t = t.tl.y +. t.size.y
let y_middle t = t.tl.y +. (t.size.y /. 2.0)

let mem pt box =
  pt.x >= box.tl.x && pt.y >= box.tl.y
  && pt.x <= box.tl.x +. box.size.x
  && pt.y <= box.tl.y +. box.size.y

let random_mem box =
  let x = Random.float (width box) +. x_left box
  and y = Random.float (height box) +. y_top box in
  Point.v x y

let shrink ?(left = 0.0) ?(right = 0.0) ?(top = 0.0) ?(bottom = 0.0) t =
  v Point.(t.tl + v left top) Size.(t.size - v (left +. right) (top +. bottom))

let pp h { tl; size } = Format.fprintf h "Box.v %a %a" Point.pp tl Size.pp size
