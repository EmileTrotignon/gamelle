open Gamelle

(* Two glyphs in isolation: the inter-letter advance is the only thing between
   them, so this isolates per-glyph advance from line layout. *)
let render ?font ~io () =
  let io = Common.setup ~io 520. 320. in
  let size = 240 and text = "EE" and at = Point.v 30. 30. in
  let s = Text.size ~io ?font ~size text in
  Common.record (Printf.sprintf "size=%d %S" size text) s;
  Box.draw ~io ~color:Color.red (Box.v at s);
  Text.draw ~io ?font ~color:Color.black ~size ~at text
