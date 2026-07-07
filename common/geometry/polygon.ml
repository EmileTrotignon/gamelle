open Xy

type t = Point.t list [@@deriving yojson]

let v li = li
let points p = p

let center = function
  | [] -> assert false
  | hd :: _ as ps ->
      let rec go ~area ~cx ~cy = function
        | [] | [ _ ] ->
            let area6 = 3.0 *. area in
            Vec.v (cx /. area6) (cy /. area6)
        | p0 :: p1 :: ps ->
            let delta = Vec.cross p0 p1 in
            go ~area:(area +. delta)
              ~cx:(cx +. ((p0.x +. p1.x) *. delta))
              ~cy:(cy +. ((p0.y +. p1.y) *. delta))
              (p1 :: ps)
      in
      go ~area:0.0 ~cx:0.0 ~cy:0.0 (hd :: List.rev ps)

let signed_area = function
  | [] -> 0.0
  | hd :: _ as ps ->
      let rec go ~area = function
        | [] | [ _ ] -> area /. 2.0
        | p0 :: p1 :: ps ->
            let delta = Vec.cross p0 p1 in
            go ~area:(area +. delta) (p1 :: ps)
      in
      go ~area:0.0 (hd :: List.rev ps)

let segments pts =
  match pts with
  | [] -> []
  | first :: _ ->
      let rec go acc = function
        | [] -> acc
        | [ last ] -> Segment.v last first :: acc
        | a :: b :: xs -> go (Segment.v a b :: acc) (b :: xs)
      in
      go [] pts

let translate v poly = List.map (Vec.( + ) v) poly
let map_points f poly = List.map f poly

let bounding_box poly =
  let x_min, x_max, y_min, y_max =
    List.fold_left
      (fun (x_min, x_max, y_min, y_max) p ->
        ( Float.min p.x x_min,
          Float.max p.x x_max,
          Float.min p.y y_min,
          Float.max p.y y_max ))
      (Float.infinity, Float.neg_infinity, Float.infinity, Float.neg_infinity)
      poly
  in
  Box.v_corners (Point.v x_min y_min) (Point.v x_max y_max)

let pp h lst =
  Format.fprintf h "Polygon.v [ %a ]"
    (Format.pp_print_list ~pp_sep:(fun h () -> Format.fprintf h " ; ") Point.pp)
    lst
