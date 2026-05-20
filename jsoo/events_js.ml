open Brr
open Gamelle_common.Events_backend

let previous = ref default
let current = ref default

let new_frame () =
  current := update_updown !previous !current;
  previous := !current

let keys_of_string kc =
  match Jstr.to_string kc with
  | "Alt" -> [ `alt ]
  | "AltGraph" -> [ `alt_gr ]
  | "ArrowLeft" -> [ `arrow_left ]
  | "ArrowRight" -> [ `arrow_right ]
  | "ArrowUp" -> [ `arrow_up ]
  | "ArrowDown" -> [ `arrow_down ]
  | "Backspace" -> [ `backspace ]
  | "CapsLock" -> [ `caps_lock ]
  | "Control" -> [ `control_left ]
  | "Delete" -> [ `delete ]
  | "End" -> [ `end_key ]
  | "Enter" -> [ `enter ]
  | "Escape" -> [ `escape ]
  | "F1" -> [ `f1 ]
  | "F2" -> [ `f2 ]
  | "F3" -> [ `f3 ]
  | "F4" -> [ `f4 ]
  | "F5" -> [ `f5 ]
  | "F6" -> [ `f6 ]
  | "F7" -> [ `f7 ]
  | "F8" -> [ `f8 ]
  | "F9" -> [ `f9 ]
  | "F10" -> [ `f10 ]
  | "F11" -> [ `f11 ]
  | "F12" -> [ `f12 ]
  | "Home" -> [ `home ]
  | "Insert" -> [ `insert ]
  | "Meta" -> [ `meta ]
  | "NumLock" -> [ `num_lock ]
  | "PageDown" -> [ `page_down ]
  | "PageUp" -> [ `page_up ]
  | "Pause" -> [ `pause ]
  | "PrintScreen" -> [ `print_screen ]
  | "ScrollLock" -> [ `scroll_lock ]
  | "Shift" -> [ `shift ]
  | " " -> [ `space; `input_char " " ]
  | "Tab" -> [ `tab ]
  | key when Jstr.length kc = 1 -> [ `input_char key ]
  | kc ->
      Console.(log [ "TODO key:"; kc ]);
      [ `unknown_key ]

let keys_of_code kc =
  match Jstr.to_string kc with
  | "ControlLeft" -> [ `control_left ]
  | "ControlRight" -> [ `control_right ]
  | "Numpad0" -> [ `kp_0 ]
  | "Numpad1" -> [ `kp_1 ]
  | "Numpad2" -> [ `kp_2 ]
  | "Numpad3" -> [ `kp_3 ]
  | "Numpad4" -> [ `kp_4 ]
  | "Numpad5" -> [ `kp_5 ]
  | "Numpad6" -> [ `kp_6 ]
  | "Numpad7" -> [ `kp_7 ]
  | "Numpad8" -> [ `kp_8 ]
  | "Numpad9" -> [ `kp_9 ]
  | "NumpadAdd" -> [ `kp_add ]
  | "NumpadDecimal" -> [ `kp_decimal ]
  | "NumpadDivide" -> [ `kp_divide ]
  | "NumpadEnter" -> [ `kp_enter ]
  | "NumpadEqual" -> [ `kp_equal ]
  | "NumpadMultiply" -> [ `kp_multiply ]
  | "NumpadSubtract" -> [ `kp_subtract ]
  | "Backquote" -> [ `physical_char '`' ]
  | "Backslash" -> [ `physical_char '\\' ]
  | "BracketLeft" -> [ `physical_char '[' ]
  | "BracketRight" -> [ `physical_char ']' ]
  | "Comma" -> [ `physical_char ',' ]
  | "Equal" -> [ `physical_char '=' ]
  | "Minus" -> [ `physical_char '-' ]
  | "Period" -> [ `physical_char '.' ]
  | "Quote" -> [ `physical_char '\'' ]
  | "Semicolon" -> [ `physical_char ';' ]
  | "Slash" -> [ `physical_char '/' ]
  | kc ->
      let c = Scanf.sscanf_opt kc "Key%c" Fun.id in
      (match c with
      | Some c -> [ `physical_char (Char.lowercase_ascii c) ]
      | None ->
          let c = Scanf.sscanf_opt kc "Digit%c" Fun.id in
          match c with
          | Some c -> [ `physical_char c ]
          | None -> [])

let keys_of_event e =
  keys_of_code (Ev.Keyboard.code e) @ keys_of_string (Ev.Keyboard.key e)

let update ~status t e =
  let keys = keys_of_event e in
  let chars =
    keys
    |> List.filter_map (fun key ->
        match key with `input_char c -> Some c | _ -> None)
    |> Strings.of_list
  in
  let keys = Keys.of_list keys in

  match status with
  | `Up ->
      {
        t with
        keypressed = Keys.diff t.keypressed keys;
        pressed_chars = Strings.diff t.pressed_chars chars;
      }
  | `Down ->
      {
        t with
        keypressed = Keys.union keys t.keypressed;
        pressed_chars = Strings.union t.pressed_chars chars;
      }

let do_update ~status e = current := update ~status !current (Ev.as_type e)

let update_mouse t e =
  let x, y = (Ev.Mouse.offset_x e, Ev.Mouse.offset_y e) in
  let buttons = Ev.Mouse.buttons e in
  let t =
    if buttons land 0x01 <> 0 then
      { t with keypressed = insert `click_left t.keypressed }
    else { t with keypressed = remove `click_left t.keypressed }
  in
  let t =
    if buttons land 0x10 <> 0 then
      { t with keypressed = insert `click_right t.keypressed }
    else { t with keypressed = remove `click_right t.keypressed }
  in
  { t with mouse_x = x; mouse_y = y }

let do_update_mouse e = current := update_mouse !current (Ev.as_type e)

let update_wheel t e =
  let delta = Ev.Wheel.delta_y e /. 4. in
  { t with keypressed = insert `wheel t.keypressed; wheel_delta = delta }

let do_update_wheel e = current := update_wheel !current (Ev.as_type e)

let attach ~target =
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "keyup"))
      (do_update ~status:`Up) target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "keydown"))
      (do_update ~status:`Down) target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousemove"))
      do_update_mouse target
  in
  let _ =
    Ev.listen (Ev.Type.create (Jstr.of_string "mouseup")) do_update_mouse target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousedown"))
      do_update_mouse target
  in
  let _ =
    Ev.listen (Ev.Type.create (Jstr.of_string "wheel")) do_update_wheel target
  in
  ()
