open Draw_geometry
open Ui_backend
open Widgets

let scroll_bar_width = 12.

type state = { child_height : float; offset : float }

module State = Ui_backend.State (struct
  type t = state
end)

let default = { child_height = 0.0; offset = 0.0 }

let with_vertical_drag ui box percent fn =
  on_click ui @@ fun state ->
  let percent =
    match state with
    | `normal | `hover -> percent
    | `pressed | `clicked ->
        let m = Event.mouse_pos ~io:(get_io ui) in
        (m.y -. Box.y_top box) /. Box.height box
  in
  let percent = max 0.0 (min 1.0 percent) in
  fn percent

let vscrollbar ui ?(min_height = 100.0) container_height state =
  with_box ui @@ fun box ->
  let h = max 1.0 container_height in
  let child_height = state.child_height in
  let child_height = max h child_height in
  let relative_height = h *. h /. child_height in
  let half = relative_height /. 2.0 in
  let drag_box = Box.shrink ~top:half ~bottom:half box in
  let max_height = child_height -. h in
  let percent = if max_height <= 0.0 then 0.0 else state.offset /. max_height in
  with_vertical_drag ui drag_box percent @@ fun percent ->
  draw ui ~min_width:scroll_bar_width ~min_height ~flex_height:1.0
    (fun ~io box ->
      let scrollbox =
        Box.v
          (Point.v (Box.x_left box)
             (Box.y_top box +. (percent *. (Box.height box -. relative_height))))
          (Size.v (Box.width box) relative_height)
      in
      Box.fill ~io ~color:lowlight box;
      Box.fill ~io ~color:highlight scrollbox);
  percent *. max_height

let v ui ?(min_height = 100.0) fn =
  boxed ~pad:0.0 ~border:Gruvbox.Light.fg2 ~bg:Gruvbox.Light.bg2 ui @@ fun () ->
  with_box ui @@ fun container_box ->
  horizontal ui ~gap:0.0 @@ fun () ->
  let state = State.find ui default in
  let new_child_height = ref !state.child_height in
  let result =
    with_box ui @@ fun clip_box ->
    vclip ui clip_box ~offset:(Vec.v 0.0 !state.offset) @@ fun () ->
    with_box ui @@ fun child_box ->
    new_child_height := Box.height child_box;
    padding ui 10.0 @@ fun () ->
    vertical ui fn
  in
  let cur = { !state with child_height = !new_child_height } in
  let offset = vscrollbar ui ~min_height (Box.height container_box) cur in
  let io = get_io ui in
  let offset =
    if
      Box.mem (Event.mouse_pos ~io) container_box
      && Event.handle_clip_events ~io true
    then
      max 0.0
        (min
           (cur.child_height -. Box.height container_box)
           (offset +. Event.wheel_delta ~io))
    else offset
  in
  state := { cur with offset };
  result
