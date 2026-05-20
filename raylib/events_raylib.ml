open Gamelle_common.Events_backend

let uchar_to_utf8 u =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let key_of_raylib : Raylib.Key.t -> key = function
  | Raylib.Key.Null -> `unknown_key
  | Raylib.Key.Apostrophe -> `physical_char '\''
  | Raylib.Key.Comma -> `physical_char ','
  | Raylib.Key.Minus -> `physical_char '-'
  | Raylib.Key.Period -> `physical_char '.'
  | Raylib.Key.Slash -> `physical_char '/'
  | Raylib.Key.Zero -> `physical_char '0'
  | Raylib.Key.One -> `physical_char '1'
  | Raylib.Key.Two -> `physical_char '2'
  | Raylib.Key.Three -> `physical_char '3'
  | Raylib.Key.Four -> `physical_char '4'
  | Raylib.Key.Five -> `physical_char '5'
  | Raylib.Key.Six -> `physical_char '6'
  | Raylib.Key.Seven -> `physical_char '7'
  | Raylib.Key.Eight -> `physical_char '8'
  | Raylib.Key.Nine -> `physical_char '9'
  | Raylib.Key.Semicolon -> `physical_char ';'
  | Raylib.Key.Equal -> `physical_char '='
  | Raylib.Key.A -> `physical_char 'a'
  | Raylib.Key.B -> `physical_char 'b'
  | Raylib.Key.C -> `physical_char 'c'
  | Raylib.Key.D -> `physical_char 'd'
  | Raylib.Key.E -> `physical_char 'e'
  | Raylib.Key.F -> `physical_char 'f'
  | Raylib.Key.G -> `physical_char 'g'
  | Raylib.Key.H -> `physical_char 'h'
  | Raylib.Key.I -> `physical_char 'i'
  | Raylib.Key.J -> `physical_char 'j'
  | Raylib.Key.K -> `physical_char 'k'
  | Raylib.Key.L -> `physical_char 'l'
  | Raylib.Key.M -> `physical_char 'm'
  | Raylib.Key.N -> `physical_char 'n'
  | Raylib.Key.O -> `physical_char 'o'
  | Raylib.Key.P -> `physical_char 'p'
  | Raylib.Key.Q -> `physical_char 'q'
  | Raylib.Key.R -> `physical_char 'r'
  | Raylib.Key.S -> `physical_char 's'
  | Raylib.Key.T -> `physical_char 't'
  | Raylib.Key.U -> `physical_char 'u'
  | Raylib.Key.V -> `physical_char 'v'
  | Raylib.Key.W -> `physical_char 'w'
  | Raylib.Key.X -> `physical_char 'x'
  | Raylib.Key.Y -> `physical_char 'y'
  | Raylib.Key.Z -> `physical_char 'z'
  | Raylib.Key.Left_bracket -> `physical_char '['
  | Raylib.Key.Backslash -> `physical_char '\\'
  | Raylib.Key.Right_bracket -> `physical_char ']'
  | Raylib.Key.Grave -> `physical_char '`'
  | Raylib.Key.Space -> `space
  | Raylib.Key.Escape -> `escape
  | Raylib.Key.Enter -> `enter
  | Raylib.Key.Tab -> `tab
  | Raylib.Key.Backspace -> `backspace
  | Raylib.Key.Insert -> `insert
  | Raylib.Key.Delete -> `delete
  | Raylib.Key.Right -> `arrow_right
  | Raylib.Key.Left -> `arrow_left
  | Raylib.Key.Down -> `arrow_down
  | Raylib.Key.Up -> `arrow_up
  | Raylib.Key.Page_up -> `page_up
  | Raylib.Key.Page_down -> `page_down
  | Raylib.Key.Home -> `home
  | Raylib.Key.End -> `end_key
  | Raylib.Key.Caps_lock -> `caps_lock
  | Raylib.Key.Scroll_lock -> `scroll_lock
  | Raylib.Key.Num_lock -> `num_lock
  | Raylib.Key.Print_screen -> `print_screen
  | Raylib.Key.Pause -> `pause
  | Raylib.Key.F1 -> `f1
  | Raylib.Key.F2 -> `f2
  | Raylib.Key.F3 -> `f3
  | Raylib.Key.F4 -> `f4
  | Raylib.Key.F5 -> `f5
  | Raylib.Key.F6 -> `f6
  | Raylib.Key.F7 -> `f7
  | Raylib.Key.F8 -> `f8
  | Raylib.Key.F9 -> `f9
  | Raylib.Key.F10 -> `f10
  | Raylib.Key.F11 -> `f11
  | Raylib.Key.F12 -> `f12
  | Raylib.Key.Left_shift -> `shift
  | Raylib.Key.Left_control -> `control_left
  | Raylib.Key.Left_alt -> `alt
  | Raylib.Key.Left_super -> `meta
  | Raylib.Key.Right_shift -> `shift
  | Raylib.Key.Right_control -> `control_right
  | Raylib.Key.Right_alt -> `alt_gr
  | Raylib.Key.Right_super -> `meta_right
  | Raylib.Key.Kb_menu -> `kb_menu
  | Raylib.Key.Kp_0 -> `kp_0
  | Raylib.Key.Kp_1 -> `kp_1
  | Raylib.Key.Kp_2 -> `kp_2
  | Raylib.Key.Kp_3 -> `kp_3
  | Raylib.Key.Kp_4 -> `kp_4
  | Raylib.Key.Kp_5 -> `kp_5
  | Raylib.Key.Kp_6 -> `kp_6
  | Raylib.Key.Kp_7 -> `kp_7
  | Raylib.Key.Kp_8 -> `kp_8
  | Raylib.Key.Kp_9 -> `kp_9
  | Raylib.Key.Kp_decimal -> `kp_decimal
  | Raylib.Key.Kp_divide -> `kp_divide
  | Raylib.Key.Kp_multiply -> `kp_multiply
  | Raylib.Key.Kp_subtract -> `kp_subtract
  | Raylib.Key.Kp_add -> `kp_add
  | Raylib.Key.Kp_enter -> `kp_enter
  | Raylib.Key.Kp_equal -> `kp_equal
  | Raylib.Key.Back -> `back
  | Raylib.Key.Menu -> `menu
  | Raylib.Key.Volume_up -> `volume_up
  | Raylib.Key.Volume_down -> `volume_down

let build_keypressed () =
  let keys = ref Keys.empty in
  for i = 0 to 350 do
    let rk = Raylib.Key.of_int i in
    if Raylib.is_key_down rk then keys := Keys.add (key_of_raylib rk) !keys
  done;
  if Raylib.is_mouse_button_down Raylib.MouseButton.Left then
    keys := Keys.add `click_left !keys;
  if Raylib.is_mouse_button_down Raylib.MouseButton.Right then
    keys := Keys.add `click_right !keys;
  let wheel = Raylib.get_mouse_wheel_move () in
  if wheel <> 0. then keys := Keys.add `wheel !keys;
  !keys

let rec collect_chars acc =
  let u = Raylib.get_char_pressed () in
  if Uchar.to_int u = 0 then acc
  else collect_chars (Strings.add (uchar_to_utf8 u) acc)

let update clock previous =
  let wheel_delta = Raylib.get_mouse_wheel_move () *. 4.0 in
  let pos = Raylib.get_mouse_position () in
  let mouse_x = Raylib.Vector2.x pos in
  let mouse_y = Raylib.Vector2.y pos in
  let pressed_chars = collect_chars Strings.empty in
  let keypressed = build_keypressed () in
  let keypressed =
    if Raylib.window_should_close () then Keys.add `quit keypressed
    else keypressed
  in
  let keypressed =
    Strings.fold
      (fun s k -> Keys.add (`input_char s) k)
      pressed_chars keypressed
  in
  let t =
    {
      previous with
      clock;
      mouse_x;
      mouse_y;
      wheel_delta;
      pressed_chars;
      keypressed;
    }
  in
  update_updown previous t
