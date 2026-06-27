open Draw_geometry
open Ui_backend

type ui = Ui_backend.ui

let horizontal ui ?(gap = 8.0) f = parent ui (Layout.horizontal ~gap) f
let vertical ui ?(gap = 8.0) f = parent ui (Layout.vertical ~gap) f
let over ui f = parent ui Layout.over f
let center ui fn = parent ui Layout.center fn
let padding ui p f = parent1 ui (Layout.pad p) f
let reshape ui ?width ?height fn = parent1 ui (Layout.reshape ?width ?height) fn

module Boxes = Ui_backend.State (struct
  type t = Box.t ref
end)

let with_box ui f =
  over ui @@ fun () ->
  let box_ref = with_state (module Boxes) ui (ref Box.zero) Fun.id in
  draw ui (fun ~io:_ box -> box_ref := box);
  f !box_ref

let label ui text =
  let size = Text.size ~io:(get_io ui) text in
  draw_size ui size @@ fun ~io box ->
  Text.draw ~io ~color:Color.black ~at:(Box.top_left box) text

let text_area ui text =
  let io = get_io ui in
  let min_width, _ = Text.min_size ~io text in
  draw_layout ui
  @@ Layout.width ~min:min_width ~flex:1.0
  @@ fun actual_width ->
  let size = Text.size_multiline ~io ~width:actual_width text in
  Layout.height ~min:(Size.height size) @@ fun box ->
  let width = Box.width box in
  Text.draw_multiline ~io ~width ~at:(Box.top_left box) ~color:Color.black text

let boxed ?(border = Gruvbox.Light.fg) ?(bg = Gruvbox.Light.bg) ?(pad = 10.0) ui
    fn =
  over ui @@ fun () ->
  draw ui (fun ~io box ->
    let radius = 4. in
      Box.fill_rounded ~io ~color:bg ~radius box;
      Box.draw_rounded ~io ~color:border ~radius box);
  padding ui pad fn

module Button_state = Ui_backend.State (struct
  type t = [ `normal | `hover | `pressed | `clicked ]
end)

let focus : Ui_backend.id option ref = ref None

let on_click ui fn =
  with_box ui @@ fun box ->
  let io = get_io ui in
  let mouse = Event.mouse_pos ~io in
  let hover = Event.handle_clip_events ~io true && Box.mem mouse box in
  let st =
    Ui_backend.with_state
      (module Button_state)
      ui `normal
      (fun state ->
        match state with
        | `normal when hover && !focus = None -> `hover
        | `hover when not hover -> `normal
        | `hover when Event.is_down ~io `click_left ->
            focus := Some (Ui_backend.id ui);
            `pressed
        | `pressed when Event.is_up ~io:(View.clip_events false io) `click_left
          ->
            focus := None;
            if hover then `clicked else `normal
        | `clicked -> `normal
        | other -> other)
  in
  fn st

let button ui text =
  reshape ui ~height:(fun h -> { h with Layout.flex = 0.1 }) @@ fun () ->
  on_click ui @@ fun state ->
  let bg =
    match state with
    | `normal -> Gruvbox.Light.bg
    | `hover -> Gruvbox.Light.bg1
    | `pressed -> Gruvbox.Light.bg2
    | `clicked -> Gruvbox.Light.bg3
  in
  boxed ~bg ui (fun () -> center ui @@ fun () -> label ui text);
  state = `clicked

let draw_checkbox ui checked state =
  draw ui ~min_width:30.0 ~min_height:30.0 @@ fun ~io box ->
    let radius = 4. in
  Box.fill_rounded ~io ~color:Gruvbox.Light.bg ~radius box;
  Box.draw_rounded ~io ~color:Gruvbox.Light.fg ~radius box;
  let p = 4.0 in
  if checked || state <> `normal then
    let color = if checked then Gruvbox.Light.blue else Gruvbox.Light.bg2 in
    Box.fill_rounded ~io ~color ~radius (Box.shrink ~left:p ~right:p ~top:p ~bottom:p box)

let checkbox ui text checked =
  on_click ui @@ fun state ->
  let checked = if state = `clicked then not checked else checked in
  horizontal ui (fun () ->
      draw_checkbox ui checked state;
      label ui text);
  checked

let draw_radio ui checked state =
  let radius = 15.0 in
  let r2 = 2.0 *. radius in
  draw ui ~min_width:r2 ~min_height:r2 @@ fun ~io box ->
  let circle = Circle.v (Box.center box) radius in
  Circle.fill ~io ~color:Gruvbox.Light.bg circle;
  Circle.draw ~io ~color:Gruvbox.Light.fg circle;
  let p = 4.0 in
  if checked || state <> `normal then
    let color = if checked then Gruvbox.Light.blue else Gruvbox.Light.bg2 in
    Circle.fill ~io ~color
      (Circle.v (Circle.center circle) (Circle.radius circle -. p))

let radio ui checked text =
  on_click ui @@ fun state ->
  horizontal ui @@ fun () ->
  let checked = checked || state = `clicked in
  draw_radio ui checked state;
  label ui text;
  checked

let radios ui ?(equal = ( = )) value options =
  List.fold_left
    (fun value (value', text) ->
      let checked = equal value value' in
      if radio ui checked text then value' else value)
    value options
