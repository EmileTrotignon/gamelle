open Gamelle_common.Geometry

type constrain = { min : float; flex : float; max : float }

module C1 = struct
  type t = constrain

  let exact min = { min; flex = 0.0; max = min }
  let zero = { min = 0.0; flex = 0.0; max = 0.0 }
  let flex = { min = 0.0; flex = 1.0; max = infinity }
  let add cst t = { t with min = t.min +. cst }

  let concat a b =
    { min = a.min +. b.min; flex = a.flex +. b.flex; max = a.max +. b.max }

  let max a b =
    { min = max a.min b.min; flex = max a.flex b.flex; max = max a.max b.max }

  let rec solve space lst =
    let used_space, total_flex =
      List.fold_left
        (fun (used_space, total_flex) t ->
          if t.flex = 0.0 then (used_space +. t.min, total_flex)
          else (used_space, total_flex +. t.flex))
        (0.0, 0.0) lst
    in
    let remaining_space = space -. used_space in
    if remaining_space <= 0.0 || total_flex = 0.0 then
      List.map (fun t -> t.min) lst
    else
      let unhappy = ref 0.0 in
      let lst =
        List.map
          (fun t ->
            if t.flex = 0.0 then (t.min, t)
            else
              let desired = remaining_space *. t.flex /. total_flex in
              let error_min = desired -. t.min in
              if error_min < 0.0 then unhappy := !unhappy +. error_min;
              let error_max = desired -. t.max in
              if error_max > 0.0 then unhappy := !unhappy +. error_max;
              (desired, t))
          lst
      in
      if abs_float !unhappy < 0.0001 then List.map fst lst
      else
        let lst =
          List.map
            (fun (desired, t) ->
              if t.flex = 0.0 then t
              else if !unhappy < 0.0 && desired <= t.min then
                { min = t.min; max = t.min; flex = 0.0 }
              else if !unhappy > 0.0 && desired >= t.max then
                { min = t.max; max = t.max; flex = 0.0 }
              else t)
            lst
        in
        solve space lst
end

type h = C1.t * (Box.t -> unit)
type t = C1.t * (float -> h)
(* width constraint, then height constraint, then render *)

let width ?(min = 0.0) ?(max = infinity) ?(flex = 0.0) fn : t =
  ({ min; flex; max }, fn)

let height ?(min = 0.0) ?(max = infinity) ?(flex = 0.0) fn : h =
  ({ min; flex; max }, fn)

let solve ?(width = fun w -> w) ?(height = fun h -> h) layout =
  let { min = w; _ }, fn = layout in
  let w = max w (width w) in
  let { min = h; _ }, fn = fn w in
  let h = max h (height h) in
  fn (Box.v Point.zero (Size.v w h))

let v ?(min_width = 0.0) ?(flex_width = 0.0) ?(max_width = infinity)
    ?(min_height = 0.0) ?(flex_height = 0.0) ?(max_height = infinity) fn =
  ( { min = min_width; flex = flex_width; max = max_width },
    fun _ -> ({ min = min_height; flex = flex_height; max = max_height }, fn) )

let fixed ?(width = 0.0) ?(height = 0.0) fn =
  (C1.exact width, fun _ -> (C1.exact height, fn))

let empty ?width ?height () = fixed ?width ?height (fun _ -> ())

let box_pad p p2 box =
  let p = Vec.v p p in
  let p2 = Vec.v p2 p2 in
  Box.v Point.(Box.top_left box + p) Size.(Box.size box - p2)

let pad p (width, fn) =
  let p2 = 2.0 *. p in
  ( C1.add p2 width,
    fun w ->
      let height, fn = fn (w -. p2) in
      (C1.add p2 height, fun box -> fn (box_pad p p2 box)) )

let horizontal lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.concat acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let widths = C1.solve actual_width (List.map fst lst) in
      let height, rev_lst =
        List.fold_left2
          (fun (acc_height, rev_lst) (_, fn) w ->
            let h, fn = fn w in
            (C1.max acc_height h, (w, fn) :: rev_lst))
          (C1.zero, []) lst widths
      in
      let lst = List.rev rev_lst in
      ( height,
        fun box ->
          let h = Box.height box in
          let _ =
            List.fold_left
              (fun box (w, fn) ->
                fn (Box.v (Box.top_left box) (Size.v w h));

                let offset = Size.v w 0.0 in
                Box.v
                  Point.(Box.top_left box + offset)
                  Size.(Box.size box - offset))
              box lst
          in
          () ) )

let vertical lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.max acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let height, rev_lst =
        List.fold_left
          (fun (acc_height, rev_lst) (_, fn) ->
            let h, fn = fn actual_width in
            (C1.concat acc_height h, (h, fn) :: rev_lst))
          (C1.zero, []) lst
      in
      let lst = List.rev rev_lst in
      ( height,
        fun box ->
          let heights = C1.solve (Box.height box) (List.map fst lst) in
          let _ =
            List.fold_left2
              (fun box (_, fn) h ->
                fn (Box.v (Box.top_left box) (Size.v (Box.width box) h));
                let offset = Size.v 0.0 h in
                Box.v
                  Point.(Box.top_left box + offset)
                  Size.(Box.size box - offset))
              box lst heights
          in
          () ) )

let rec intersperse e li =
  match li with
  | [] -> []
  | [ elt ] -> [ elt ]
  | elt :: li -> elt :: e :: intersperse e li

let horizontal ?gap lst =
  let lst =
    match gap with
    | Some gap when gap > 0.0 -> intersperse (empty ~width:gap ()) lst
    | _ -> lst
  in
  horizontal lst

let vertical ?gap lst =
  let lst =
    match gap with
    | Some gap when gap > 0.0 -> intersperse (empty ~height:gap ()) lst
    | _ -> lst
  in
  vertical lst

let over lst =
  let width =
    List.fold_left (fun acc_width (w, _) -> C1.max acc_width w) C1.zero lst
  in
  ( width,
    fun actual_width ->
      let height, rev_lst =
        List.fold_left
          (fun (acc_height, rev_lst) (_, fn) ->
            let h, fn = fn actual_width in
            (C1.max acc_height h, fn :: rev_lst))
          (C1.zero, []) lst
      in
      let lst = List.rev rev_lst in
      (height, fun box -> List.iter (fun fn -> fn box) lst) )

let reshape ?(width = fun t -> t) ?(height = fun t -> t) layout =
  let w_constraint, fn = layout in
  ( width w_constraint,
    fun actual_width ->
      let h_constraint, fn = fn actual_width in
      (height h_constraint, fn) )

let vclip height layout =
  let w_constraint, fn = layout in
  ( w_constraint,
    fun actual_width ->
      let h_constraint, fn = fn actual_width in
      ( C1.exact height,
        fun box ->
          let box =
            Box.v (Box.top_left box)
              (Size.v (Box.width box) (max h_constraint.min (Box.height box)))
          in
          fn box ) )

let hclip width layout =
  let w_constraint, fn = layout in
  ( C1.exact width,
    fun actual_width ->
      let h_constraint, fn = fn (max w_constraint.min actual_width) in
      ( h_constraint,
        fun box ->
          let box =
            Box.v (Box.top_left box)
              (Size.v (max w_constraint.min (Box.width box)) (Box.height box))
          in
          fn box ) )

let center layout =
  let w, fn = layout in
  ( C1.max w C1.flex,
    fun actual_width ->
      let h, fn = fn actual_width in
      ( C1.max h C1.flex,
        fun box ->
          let width = if w.flex = 0.0 then w.min else Box.width box in
          let height = if h.flex = 0.0 then h.min else Box.height box in
          let box = Box.v_center (Box.center box) (Size.v width height) in
          fn box ) )

let center layout = center (vertical layout)
