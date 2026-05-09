open Draw_geometry
open Ui_backend
open Widgets

type state = {
  offset : float;
  cursor : int;
  focused : bool;
  pressed_key : (int * Event.key) option;
}

let default_state =
  { offset = 0.; cursor = 0; focused = false; pressed_key = None }

let text_length ~io text = Size.width (Text.size_t ~io text)
let cursor_offset ~io text cursor = text_length ~io (Text.sub text 0 cursor)

let delete_char i text =
  Text.(sub text 0 i ^ sub text (i + 1) (length text - (i + 1)))

let render ~io { offset; cursor; focused; pressed_key = _ } text box =
  let p = -4.0 in
  let box' = Box.shrink ~left:p ~right:p ~top:p ~bottom:p box in
  let io = View.clip (Box.translate (Vec.v offset 0.0) box') io in
  let io = View.translate (Vec.v (-.offset) 0.0) io in
  Text.draw_t ~io ~color:fg ~at:(Box.top_left box) text;
  if focused then
    let cursor_pos = cursor_offset ~io text cursor in
    let cursor_seg =
      Segment.v
        (Point.v (Box.x_left box +. cursor_pos) (Box.y_top box))
        (Point.v
           (Box.x_left box +. cursor_pos)
           (Box.y_top box +. Size.height (Text.size ~io "a")))
    in
    Segment.draw ~io ~color:highlight cursor_seg

let find_cursor_click ~io text x =
  let rec loop prev_w i =
    if i >= Text.length text then i
    else
      let subtext = Text.sub text 0 i in
      let w = text_length ~io subtext in
      if w > x then if w -. x < x -. prev_w then i else i - 1 else loop w (i + 1)
  in
  loop 0. 0

let slow_frames = 60
let slow_frequency = 20
let fast_frequency = 3

let update ~io { offset; cursor; focused; pressed_key } text box =
  let width = max (Box.width box) 0. in
  let pressed_key =
    match pressed_key with
    | Some (_, key) when Event.is_up ~io key -> None
    | k -> k
  in
  let pressed_key =
    Option.map (fun (frames, k) -> (frames + 1, k)) pressed_key
  in
  let pressed_key =
    if Event.is_down ~io `arrow_left then Some (0, `arrow_left)
    else if Event.is_down ~io `arrow_right then Some (0, `arrow_right)
    else if Event.is_down ~io `backspace then Some (0, `backspace)
    else pressed_key
  in
  let module Strings = Event.Strings in
  let pressed_key =
    let char_down = Event.down_chars ~io |> Strings.choose_opt in
    match char_down with
    | Some c -> Some (0, `input_char c)
    | None -> pressed_key
  in
  let cursor, focused =
    if Event.is_up ~io `click_left then
      if Box.mem (Event.mouse_pos ~io) box then
        let x = (Event.mouse_pos ~io).x -. Box.x_left box +. offset in
        (find_cursor_click ~io text x, true)
      else (cursor, false)
    else (cursor, focused)
  in
  let cursor =
    Int.max 0
      (Int.min (Text.length text)
         (match pressed_key with
         | None -> cursor
         | Some (0, ((`arrow_left | `arrow_right) as dir)) -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | Some (n, ((`arrow_left | `arrow_right) as dir))
           when n < slow_frames && n mod slow_frequency = 0 -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | Some (n, ((`arrow_left | `arrow_right) as dir))
           when n >= slow_frames && n mod fast_frequency = 0 -> (
             cursor + match dir with `arrow_left -> -1 | `arrow_right -> 1)
         | _ -> cursor))
  in
  let char_to_add =
    match pressed_key with
    | None -> None
    | Some (0, `input_char char) -> Some char
    | Some (n, `input_char char)
      when n < slow_frames && n mod slow_frequency = 0 ->
        Some char
    | Some (n, `input_char char)
      when n >= slow_frames && n mod fast_frequency = 0 ->
        Some char
    | _ -> None
  in
  let cursor, text =
    match char_to_add with
    | None -> (cursor, text)
    | Some char ->
        ( cursor + 1,
          Text.(
            sub text 0 cursor ^ of_string char
            ^ sub text cursor (length text - cursor)) )
  in
  let cursor, text =
    match pressed_key with
    | None -> (cursor, text)
    | Some (0, `backspace) when cursor > 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | Some (n, `backspace)
      when cursor > 0 && n < slow_frames && n mod slow_frequency = 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | Some (n, `backspace)
      when cursor > 0 && n >= slow_frames && n mod fast_frequency = 0 ->
        let cursor = cursor - 1 in
        (cursor, delete_char cursor text)
    | _ -> (cursor, text)
  in

  let cursor_pos = cursor_offset ~io text cursor in
  let max_width = Size.width (Text.size_t ~io text) in
  let offset =
    if cursor_pos -. 30.0 < offset then max 0.0 (cursor_pos -. 30.0)
    else if cursor_pos +. 30.0 > offset +. width then
      cursor_pos +. 30.0 -. width
    else offset
  in
  let offset = max 0.0 (min (max_width -. width) offset) in
  ({ offset; cursor; focused; pressed_key }, text)

module State = Ui_backend.State (struct
  type t = state * Text.t
end)

let v ui text =
  boxed ui @@ fun () ->
  with_box ui @@ fun box ->
  let io = get_io ui in
  let default = (default_state, Text.of_string text) in
  let _internal, text =
    Ui_backend.with_state (module State) ui default begin fun (st, text) ->
      let st, text = update ~io st text box in
      let text_size = Text.size_t ~io text in
      Ui_backend.draw ui ~min_width:30.0 ~flex_width:1.0
        ~min_height:(Size.height text_size) (render st text);
      (st, text)
      end
  in
  Text.to_string text
