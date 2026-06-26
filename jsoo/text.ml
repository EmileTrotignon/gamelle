include Jstr
open Draw

let ( ^ ) = append

let draw_glyph ~io ?color ?font ?size ~at txt =
  transform ~io;
  set_color ~io color;
  Font_.draw_at ~io ?color ?font ?size (Jstr.to_string txt) ~at

let size ~io ?font ?size txt =
  Font_.text_size ~io ?font ?size (Jstr.to_string txt)

let get = get_jstr
let chars txt = List.init (length txt) (fun i -> get txt i)
