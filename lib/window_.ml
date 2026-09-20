open Gamelle_common
open Geometry
module W = Gamelle_backend.Window

let size ~io = W.size ~io

let set_size ~io s =
  let w = s |> Size.width |> int_of_float
  and h = s |> Size.height |> int_of_float in
  match io.render_target with
  | Gamelle_common.View.Onscreen o -> o.requested_size <- Some (w, h)
  | Gamelle_common.View.Texture _ -> () (* an offscreen texture has a fixed size; ignore *)

let box ~io = Box.v Vec.zero (size ~io)
let show_cursor = W.show_cursor
let capture_mouse = W.capture_mouse
let is_mouse_captured = W.is_mouse_captured
let set_fullscreen = W.set_fullscreen
let get_fullscreen = W.get_fullscreen
