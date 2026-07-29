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
    (* [clip] is a screen-space polygon, so test the raw (un-projected) mouse
       position against it. *)
    | Some clip ->
        if Polygon.mem (Events_backend.mouse_pos !(io.event)) clip then b
        else false
  else b

let is_pressed ~io k =
  handle_clip_events ~io @@ Events_backend.is_pressed !(io.event) k

let is_up ~io k = handle_clip_events ~io @@ Events_backend.is_up !(io.event) k

let is_down ~io k =
  handle_clip_events ~io @@ Events_backend.is_down !(io.event) k

let mouse_delta ~io =
  Transform.inv_project_vector io.view (Events_backend.mouse_delta !(io.event))

let wheel_delta ~io = Events_backend.wheel_delta !(io.event)
let pressed_chars ~io = !(io.event).pressed_chars
let down_chars ~io = !(io.event).down_chars
let up_chars ~io = !(io.event).up_chars

let snapshot ~(io : Gamelle_backend.io) =
  let mouse_pos = mouse_pos ~io in
  let mouse_x = Point.x mouse_pos and mouse_y = Point.y mouse_pos in
  let mouse_delta = mouse_delta ~io in
  let mouse_dx = Vec.x mouse_delta and mouse_dy = Vec.y mouse_delta in
  { !(io.Gamelle_common.event) with mouse_x; mouse_y; mouse_dx; mouse_dy }
