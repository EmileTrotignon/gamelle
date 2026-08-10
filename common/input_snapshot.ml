(* Mirror of [Event] (see lib/event.ml), but reading from an explicit,
   deserialized event value instead of [~io]. Useful on the network side
   (e.g. a multiplayer server) where there is no rendering [io], only the
   event datastructure received from a client.

   Since there is no view transform or clipping here, [mouse_pos] returns the
   raw coordinates carried by the event. *)

type t = Events_backend.t [@@deriving yojson]
type key = Events_backend.key [@@deriving yojson]

module Strings = Events_backend.Strings
module Keys = Events_backend.Keys

let mouse_pos (t : t) = Events_backend.mouse_pos t
let mouse_delta (t : t) = Events_backend.mouse_delta t
let is_pressed (t : t) k = Events_backend.is_pressed t k
let is_up (t : t) k = Events_backend.is_up t k
let is_down (t : t) k = Events_backend.is_down t k
let wheel_delta (t : t) = Events_backend.wheel_delta t
let pressed_chars (t : t) = t.Events_backend.pressed_chars
let down_chars (t : t) = t.Events_backend.down_chars
let up_chars (t : t) = t.Events_backend.up_chars

let assume_next t =
  Events_backend.
    {
      t with
      Events_backend.keyup = Keys.empty;
      Events_backend.keydown = Keys.empty;
      down_chars = Strings.empty;
      up_chars = Strings.empty;
      mouse_x = t.mouse_x +. t.mouse_dx;
      mouse_y = t.mouse_y +. t.mouse_dy;
      time = t.time +. target_dt;
    }

let null = Events_backend.default
