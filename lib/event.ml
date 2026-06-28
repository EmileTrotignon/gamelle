open Gamelle_common
open Draw_geometry

type key = Events_backend.key

let key_to_yojson = Events_backend.key_to_yojson
let key_of_yojson = Events_backend.key_of_yojson

module Strings = Events_backend.Strings

let mouse_pos ~io =
  Transform.inv_project io.view (Events_backend.mouse_pos !(io.event))

let handle_clip_events ~io b =
  if io.clip_events then
    match io.clip with
    | None -> b
    | Some clip -> if Box.mem (mouse_pos ~io) clip then b else false
  else b

let is_pressed ~io k =
  handle_clip_events ~io @@ Events_backend.is_pressed !(io.event) k

let is_up ~io k = handle_clip_events ~io @@ Events_backend.is_up !(io.event) k

let is_down ~io k =
  handle_clip_events ~io @@ Events_backend.is_down !(io.event) k

let wheel_delta ~io = Events_backend.wheel_delta !(io.event)
let pressed_chars ~io = !(io.event).pressed_chars
let down_chars ~io = !(io.event).down_chars
let up_chars ~io = !(io.event).up_chars
