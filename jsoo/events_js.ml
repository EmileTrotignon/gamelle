open Brr
open Gamelle_common.Events_backend

let previous = ref default
let current = ref default

(* Pointer lock (relative mouse mode). Browsers only grant it from inside a
   user gesture handler, so [set_capture true] both tries immediately (it can
   succeed if the current frame was reached with a recent-enough gesture) and
   records the intent so the mousedown listener below can retry. The browser
   can also drop the lock at any time (Escape); the next click then re-acquires
   it. *)
let capture = ref false
let capture_el : El.t option ref = ref None
let is_locked () = Option.is_some (Document.pointer_lock_element G.document)

let request_lock () =
  match !capture_el with
  | Some el when !capture && not (is_locked ()) ->
      ignore (El.request_pointer_lock el)
  | _ -> ()

let set_capture status =
  capture := status;
  if status then request_lock ()
  else if is_locked () then ignore (Document.exit_pointer_lock G.document)

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
  | "AudioVolumeDown" -> [ `volume_down ]
  | "AudioVolumeUp" -> [ `volume_up ]
  | "Backspace" -> [ `backspace ]
  | "CapsLock" -> [ `caps_lock ]
  | "ContextMenu" -> [ `context_menu ]
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
  | "MetaLeft" -> [ `meta ]
  | "MetaRight" -> [ `meta_right ]
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
  | kc -> (
      let c = Scanf.sscanf_opt kc "Key%c" Fun.id in
      match c with
      | Some c -> [ `physical_char (Char.lowercase_ascii c) ]
      | None -> (
          let c = Scanf.sscanf_opt kc "Digit%c" Fun.id in
          match c with Some c -> [ `physical_char c ] | None -> []))

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

(* Mouse events report CSS pixels relative to the canvas element; convert to
   the game's logical coordinates. The bitmap is displayed letterboxed inside
   the element (object-fit: contain, see Window.set_size): uniformly scaled by
   [fit] and centered, so the mapping is a scale plus the letterbox offset. *)
let element_to_logical () =
  match !capture_el with
  | None -> (1.0, 0.0, 0.0)
  | Some el ->
      let lw, lh = !Jsoo.logical_size in
      let lw = float lw and lh = float lh in
      let cw = El.inner_w el and ch = El.inner_h el in
      if cw <= 0. || ch <= 0. then (1.0, 0.0, 0.0)
      else
        let fit = Float.min (cw /. lw) (ch /. lh) in
        (fit, 0.5 *. (cw -. (fit *. lw)), 0.5 *. (ch -. (fit *. lh)))

(* Button state is derived from the [buttons] bitmask, but only from
   [mousedown]/[mouseup] events, where it is reliable. It must NOT be updated
   from [mousemove]: while a pointer lock is held, browsers intermittently
   dispatch move events reporting [buttons = 0] even though the button is
   physically down. Clearing [click_left] on such a stray move made [is_pressed
   `click_left] flicker off, which broke held-fire: full-auto weapons (read via
   [is_pressed]) fired a few frames then stopped until the button was pressed
   again. *)
let update_mouse_buttons t e =
  let buttons = Ev.Mouse.buttons e in
  let set mask key t =
    if buttons land mask <> 0 then
      { t with keypressed = insert key t.keypressed }
    else { t with keypressed = remove key t.keypressed }
  in
  t |> set 0x01 `click_left |> set 0x02 `click_right

let update_mouse_pos t e =
  let fit, ox, oy = element_to_logical () in
  let x = (Ev.Mouse.offset_x e -. ox) /. fit in
  let y = (Ev.Mouse.offset_y e -. oy) /. fit in
  (* Movements accumulate over the mouse events of the current frame; the
     per-frame delta is reset by the run loop (see [reset_mouse_delta]). *)
  {
    t with
    mouse_x = x;
    mouse_y = y;
    mouse_dx = t.mouse_dx +. (Ev.Mouse.movement_x e /. fit);
    mouse_dy = t.mouse_dy +. (Ev.Mouse.movement_y e /. fit);
  }

let do_update_mouse_move e = current := update_mouse_pos !current (Ev.as_type e)

let do_update_mouse_button e =
  let e = Ev.as_type e in
  current := update_mouse_pos (update_mouse_buttons !current e) e

let update_wheel t e =
  let delta = Ev.Wheel.delta_y e /. 4. in
  { t with keypressed = insert `wheel t.keypressed; wheel_delta = delta }

let do_update_wheel e = current := update_wheel !current (Ev.as_type e)

let attach ~canvas =
  capture_el := Some canvas;
  let target = El.as_target canvas in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousedown"))
      (fun _ -> request_lock ())
      target
  in
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
      do_update_mouse_move target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mouseup"))
      do_update_mouse_button target
  in
  let _ =
    Ev.listen
      (Ev.Type.create (Jstr.of_string "mousedown"))
      do_update_mouse_button target
  in
  let _ =
    Ev.listen (Ev.Type.create (Jstr.of_string "wheel")) do_update_wheel target
  in
  ()
