open Gamelle

(* Lines of text at several sizes, plus a wrapped paragraph boxed by
   [Text.size_multiline] — exercises single-line and multi-line size prediction. *)

let lines =
  [
    "Hagqy 0123";
    "The quick brown fox";
    "AVAW To Yo. Wa";
    "abcdefghijklmnopqrstuvwxyz";
  ]

let sizes = [ 16; 20; 28 ]

let paragraph =
  "The quick brown fox jumps over the lazy dog. Pack my box. The quick brown \
   fox jumps over the lazy dog. Pack my box."

let render ?font ~io () =
  let io = Common.setup ~io Common.width Common.height in
  let y = ref 10. in
  List.iter
    (fun size ->
      List.iter
        (fun text ->
          let h =
            Common.draw_sample ~io ?font ~at:(Point.v 10. !y) ~size text
          in
          y := !y +. h +. 6.)
        lines)
    sizes;
  let mwidth = 300. and size = 20 in
  let at = Point.v 10. !y in
  let s = Text.size_multiline ~io ?font ~width:mwidth ~size paragraph in
  Common.record
    (Printf.sprintf "multiline size=%d width=%g %S" size mwidth paragraph)
    s;
  Box.draw ~io ~color:Color.red (Box.v at s);
  Text.draw_multiline ~io ?font ~color:Color.black ~width:mwidth ~size ~at
    paragraph
