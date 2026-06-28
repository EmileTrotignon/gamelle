type t = Segment of Segment.t | Circle of Circle.t | Polygon of Polygon.t
[@@deriving yojson]

let segment seg = Segment seg
let circle c = Circle c
let polygon pts = Polygon pts

let rect box =
  polygon
    (Polygon.v
       [
         Box.top_left box;
         Box.top_right box;
         Box.bottom_right box;
         Box.bottom_left box;
       ])

let translate dxy = function
  | Segment s -> Segment (Segment.translate dxy s)
  | Circle c -> Circle (Circle.translate dxy c)
  | Polygon p -> Polygon (Polygon.translate dxy p)

let ( + ) = ( +. )
let ( - ) = ( -. )
let ( ~- ) = ( ~-. )
let ( * ) = ( *. )
let ( / ) = ( /. )

let rotate_around ~angle ~center shape =
  let rot = Vec.rotate_around ~angle ~center in
  match shape with
  | Circle c -> Circle (Circle.map_center rot c)
  | Segment s -> Segment (Segment.map_points rot s)
  | Polygon pts -> Polygon (Polygon.map_points rot pts)

let center = function
  | Circle c -> Circle.center c
  | Segment s -> Segment.center s
  | Polygon pts -> Polygon.center pts

let signed_area = function
  | Circle c -> Circle.area c
  | Segment _ -> 1.0
  | Polygon pts -> Polygon.signed_area pts

let rotate ?center:opt_center angle shape =
  let center =
    match opt_center with Some center -> center | None -> center shape
  in
  rotate_around ~angle:(cos angle, sin angle) ~center shape

let nearest_point_segment pt s =
  let p0, p1 = Segment.to_tuple s in
  let l2 = Vec.(norm2 (p0 - p1)) in
  if l2 = 0.0 then p0
  else
    let t = max 0. (min 1. (Vec.(dot (pt - p0) (p1 - p0)) / l2)) in
    Vec.(p0 + (t * (p1 - p0)))

let float_equal a b = abs_float (a -. b) < 0.01

let nearest_points pt = function
  | Circle c ->
      let center = Circle.center c and radius = Circle.radius c in
      let d = Vec.(unit (pt - center)) in
      [ (Vec.(center + (radius * d)), d) ]
  | Segment s ->
      let p0, p1 = Segment.to_tuple s in
      let pt = nearest_point_segment pt s in
      [ (pt, Vec.(unit @@ ortho (p0 - p1))) ]
  | Polygon pts -> (
      match Polygon.segments pts with
      | [] -> invalid_arg "empty polygon"
      | first :: ss ->
          let compute segment =
            let p0, p1 = Segment.to_tuple segment in
            let found = nearest_point_segment pt segment in
            let dist2 = Vec.(norm2 (pt - found)) in
            (dist2, [ (found, Vec.(unit @@ ortho (p0 - p1))) ])
          in
          let _, found =
            List.fold_left
              (fun (best_distance2, bests) segment ->
                let dist2, found = compute segment in
                if float_equal dist2 best_distance2 then (dist2, found @ bests)
                else if dist2 < best_distance2 then (dist2, found)
                else (best_distance2, bests))
              (compute first) ss
          in
          found)

let distance2 pt shape =
  match nearest_points pt shape with
  | [] -> invalid_arg "distance2"
  | (npt, _) :: _ -> Vec.(norm2 (pt - npt))

let intersection_segment_circle s c =
  let p0, p1 = Segment.to_tuple s in
  let center = Circle.center c and radius = Circle.radius c in
  let d = Vec.(p1 - p0) in
  let f = Vec.(p0 - center) in
  let a = Vec.dot d d in
  let b = 2.0 * Vec.dot f d in
  let c = Vec.dot f f - (radius * radius) in
  let discriminant = (b * b) - (4. * a * c) in
  if discriminant < 0. then []
  else
    let discriminant = sqrt discriminant in
    let t1 = (-b - discriminant) / (2. * a) in
    let t2 = (-b + discriminant) / (2. * a) in
    let pt1 = if t1 >= 0. && t1 <= 1.0 then [ Vec.(p0 + (t1 * d)) ] else [] in
    let pt2 = if t2 >= 0. && t2 <= 1. then [ Vec.(p0 + (t2 * d)) ] else [] in
    pt1 @ pt2

let rec intersections a b =
  match (a, b) with
  | Segment s, Circle c | Circle c, Segment s -> intersection_segment_circle s c
  | Segment p, Segment q -> (
      match Segment.intersection p q with None -> [] | Some pt -> [ pt ])
  | Circle c, Circle c' -> Circle.intersections c c'
  | Polygon pts, other | other, Polygon pts ->
      List.concat_map
        (fun s -> intersections (Segment s) other)
        (Polygon.segments pts)

let intersect a b =
  match (a, b) with
  | Circle c, Circle c' -> Circle.intersect c c'
  | _ -> intersections a b <> []

let mem pt = function
  | Circle c -> Circle.mem pt c
  | Segment _ -> false
  | Polygon pts ->
      let x, y = (pt.x, pt.y) in
      List.fold_left
        (fun inside s ->
          let p0, p1 = Segment.to_tuple s in
          let vx0, vy0 = (p0.x, p0.y) in
          let vx1, vy1 = (p1.x, p1.y) in
          if
            vy0 > y <> (vy1 > y)
            && x < ((vx0 - vx1) * (y - vy1) / (vy0 - vy1)) + vx1
          then not inside
          else inside)
        false (Polygon.segments pts)

let separation_axis_circle c shape =
  let center = Circle.center c and radius = Circle.radius c in
  let pt, _ = List.hd @@ nearest_points center shape in
  let axis = Vec.(center - pt) in
  let dist2 = Vec.(norm2 axis) in
  if mem center shape then
    let delta = radius +. sqrt dist2 in
    Some Vec.(delta * unit axis)
  else if dist2 > radius * radius then None
  else
    let delta = radius -. sqrt dist2 in
    Some Vec.(delta * unit axis)

let project axis = function
  | [] -> invalid_arg "project: empty list"
  | pt :: pts ->
      let v = Vec.dot pt axis in
      List.fold_left
        (fun (vmin, vmax) pt ->
          let v = Vec.dot pt axis in
          (min v vmin, max v vmax))
        (v, v) pts

let separation_axis_polygon poly_a poly_b =
  let a = Polygon.points poly_a and b = Polygon.points poly_b in
  let rec go opt_best = function
    | [] -> opt_best
    | seg :: rest -> (
        let p0, p1 = Segment.to_tuple seg in
        let axis = Vec.(unit @@ ortho (p0 - p1)) in
        let amin, amax = project axis a in
        let bmin, bmax = project axis b in
        let d1 = bmin -. amax in
        let d2 = bmax -. amin in
        if d1 >= 0.0 then None
        else if d2 <= 0.0 then None
        else
          let overlap = if abs_float d1 < abs_float d2 then d1 else d2 in
          match opt_best with
          | None -> go (Some (overlap, axis)) rest
          | Some (best, _) when abs_float overlap < abs_float best ->
              go (Some (overlap, axis)) rest
          | _ -> go opt_best rest)
  in
  let ss =
    List.rev_append (Polygon.segments poly_a) (Polygon.segments poly_b)
  in
  match go None ss with None -> None | Some (d, n) -> Some Vec.(d * n)

let separation_axis a b =
  match (a, b) with
  | Circle c, shape -> separation_axis_circle c shape
  | shape, Circle c -> (
      match separation_axis_circle c shape with
      | None -> None
      | Some n -> Some Vec.(-1.0 * n))
  | Polygon a, Polygon b -> separation_axis_polygon a b
  | Segment s, Polygon b ->
      let a0, a1 = Segment.to_tuple s in
      separation_axis_polygon (Polygon.v [ a0; a1 ]) b
  | Polygon a, Segment s ->
      let b0, b1 = Segment.to_tuple s in
      separation_axis_polygon a (Polygon.v [ b0; b1 ])
  | Segment s, Segment s' ->
      let a0, a1 = Segment.to_tuple s in
      let b0, b1 = Segment.to_tuple s' in
      separation_axis_polygon (Polygon.v [ a0; a1 ]) (Polygon.v [ b0; b1 ])

let separation_axis a b =
  match separation_axis a b with
  | Some n when Vec.norm2 n = 0.0 -> None
  | found -> found

module Set_v2 = Set.Make (Vec)

let best_distance (x_dist, xs) (y_dist, ys) =
  if float_equal x_dist y_dist then (x_dist, Set_v2.union xs ys)
  else if x_dist < y_dist then (x_dist, xs)
  else (y_dist, ys)

let contact_points_rectangle pts rect =
  let pts = Polygon.points pts in
  match pts with
  | [] -> invalid_arg "contact_points: empty list"
  | hd :: pts ->
      let compute pt =
        let found = nearest_points pt rect in
        let npt, _ = List.hd found in
        let dist2 = Vec.(norm2 (npt - pt)) in
        (dist2, Set_v2.singleton pt)
      in
      List.fold_left
        (fun acc pt -> best_distance acc (compute pt))
        (compute hd) pts

let rec contact_points a b =
  match (a, b) with
  | Circle c, Circle c' ->
      let a = Circle.center c and b = Circle.center c' in
      let dist = Vec.(norm (a - b)) in
      (dist, Set_v2.singleton Vec.(a + (0.5 * (a - b))))
  | Polygon a, Circle c | Circle c, Polygon a ->
      let b = Circle.center c in
      let a, _ = List.hd @@ nearest_points b (Polygon a) in
      let dist = Vec.(norm (a - b)) in
      (dist, Set_v2.singleton a)
  | Polygon a_pts, Polygon b_pts ->
      let fst = contact_points_rectangle a_pts b in
      let snd = contact_points_rectangle b_pts a in
      best_distance fst snd
  | Segment seg, b ->
      let a0, a1 = Segment.to_tuple seg in
      contact_points (Polygon (Polygon.v [ a0; a1 ])) b
  | a, Segment seg ->
      let b0, b1 = Segment.to_tuple seg in
      contact_points a (Polygon (Polygon.v [ b0; b1 ]))

let contact_points a b =
  let dist, lst = contact_points a b in
  (dist, Set_v2.elements lst)

let pp h = function
  | Segment t -> Segment.pp h t
  | Circle t -> Circle.pp h t
  | Polygon t -> Polygon.pp h t
