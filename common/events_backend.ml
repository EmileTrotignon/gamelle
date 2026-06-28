open Geometry

type key =
  [ `alt
  | `alt_gr
  | `arrow_down
  | `arrow_left
  | `arrow_right
  | `arrow_up
  | `backspace
  | `caps_lock
  | `click_left
  | `click_right
  | `control_left
  | `control_right
  | `delete
  | `end_key
  | `enter
  | `escape
  | `f1
  | `f2
  | `f3
  | `f4
  | `f5
  | `f6
  | `f7
  | `f8
  | `f9
  | `f10
  | `f11
  | `f12
  | `home
  | `input_char of string
  | `insert
  | `context_menu
  | `kp_0
  | `kp_1
  | `kp_2
  | `kp_3
  | `kp_4
  | `kp_5
  | `kp_6
  | `kp_7
  | `kp_8
  | `kp_9
  | `kp_add
  | `kp_decimal
  | `kp_divide
  | `kp_enter
  | `kp_equal
  | `kp_multiply
  | `kp_subtract
  | `meta
  | `meta_right
  | `num_lock
  | `page_down
  | `page_up
  | `pause
  | `physical_char of char
  | `print_screen
  | `quit
  | `scroll_lock
  | `shift
  | `space
  | `tab
  | `volume_down
  | `volume_up
  | `wheel
  | `unknown_key ]
[@@deriving yojson]

module Strings = Set.Make (String)

module Keys = Set.Make (struct
  type t = key

  let compare a b = Stdlib.compare a b
end)

(* Sets have no automatic deriver; serialize them through their element lists. *)
let keys_to_yojson set = [%to_yojson: key list] (Keys.elements set)
let keys_of_yojson json = Result.map Keys.of_list ([%of_yojson: key list] json)
let strings_to_yojson set = [%to_yojson: string list] (Strings.elements set)

let strings_of_yojson json =
  Result.map Strings.of_list ([%of_yojson: string list] json)

type t = {
  keyup : Keys.t; [@to_yojson keys_to_yojson] [@of_yojson keys_of_yojson]
  keydown : Keys.t; [@to_yojson keys_to_yojson] [@of_yojson keys_of_yojson]
  keypressed : Keys.t; [@to_yojson keys_to_yojson] [@of_yojson keys_of_yojson]
  mouse_x : float;
  mouse_y : float;
  wheel_delta : float;
  pressed_chars : Strings.t;
      [@to_yojson strings_to_yojson] [@of_yojson strings_of_yojson]
  down_chars : Strings.t;
      [@to_yojson strings_to_yojson] [@of_yojson strings_of_yojson]
  up_chars : Strings.t;
      [@to_yojson strings_to_yojson] [@of_yojson strings_of_yojson]
  clock : int;
}
[@@deriving yojson]

let mouse_pos t = Point.v t.mouse_x t.mouse_y

let default =
  {
    keyup = Keys.empty;
    keydown = Keys.empty;
    keypressed = Keys.empty;
    mouse_x = 0.0;
    mouse_y = 0.0;
    wheel_delta = 0.;
    pressed_chars = Strings.empty;
    down_chars = Strings.empty;
    up_chars = Strings.empty;
    clock = 0;
  }

let desired_fps = 60.0
let desired_dt = 1. /. desired_fps
let dt (_ : t) = desired_dt
let clock t = float t.clock /. desired_fps
let insert = Keys.add
let remove = Keys.remove
let diff = Keys.diff
let union = Keys.union
let is_pressed t key = Keys.mem key t.keypressed
let is_up t key = Keys.mem key t.keyup
let is_down t key = Keys.mem key t.keydown

let update_updown previous t =
  let keyup = Keys.diff previous.keypressed t.keypressed in
  let keydown = Keys.diff t.keypressed previous.keypressed in
  let up_chars = Strings.diff previous.pressed_chars t.pressed_chars in
  let down_chars = Strings.diff t.pressed_chars previous.pressed_chars in
  { t with keyup; keydown; up_chars; down_chars }

let wheel_delta t = t.wheel_delta

let reset_wheel t =
  { t with keypressed = remove `wheel t.keypressed; wheel_delta = 0. }
