module Delayed = Gamelle_common.Delayed
module Transform = Gamelle_common.Transform
open Gamelle_common
open Geometry

type sized_font = {
  mutable raylib_font : Raylib.Font.t;
  codepoint_set : (int, unit) Hashtbl.t;
}

type font_s = {
  data : string;
  sizes : (int, sized_font) Hashtbl.t;
  metrics : Font_metrics.t;
      (** shared sizing/advance metrics, see Font_metrics *)
}

type io_backend = { font : font; font_size : int }
and io = io_backend abstract_io
and font = (io, font_s) Delayed.t

let clock = Gamelle_common.clock
let dt = Gamelle_common.dt
let get_font_opt ~io = function Some f -> f | None -> io.backend.font

let get_font_size_opt ~io = function
  | Some s -> s
  | None -> io.backend.font_size

let get_font ~io font_opt size_opt =
  (get_font_opt ~io font_opt, get_font_size_opt ~io size_opt)

let to_raylib_color c =
  let r, g, b, a = Color.to_srgbi c in
  let a = int_of_float (a *. 255.) in
  Raylib.Color.create r g b a

let project ~io p =
  let v = Transform.project io.view p in
  Vec.to_tuple v

let with_scissor ~io f =
  match io.clip with
  | None -> f ()
  | Some clip ->
      let clip = Transform.project_box io.view clip in
      let x = int_of_float (Box.x_left clip) in
      let y = int_of_float (Box.y_top clip) in
      let w = int_of_float (Box.width clip) in
      let h = int_of_float (Box.height clip) in
      Raylib.begin_scissor_mode x y w h;
      f ();
      Raylib.end_scissor_mode ()
