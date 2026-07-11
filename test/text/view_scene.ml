open Gamelle

(* Text under scaled and rotated views. The red boxes ([Text.size], measured in
   world coordinates) are projected through the same view transform as the
   glyphs, so the text must stay inside its box whatever the transform — a
   backend that ignores the view scale or rotation overflows visibly. *)

let size = 20

let sample ~io ?font ~at text =
  ignore (Common.draw_sample ~io ?font ~at ~size text)

let render ?font ~io () =
  let io = Common.setup ~io Common.width Common.height in
  sample ~io ?font ~at:(Point.v 10. 10.) "identity view";
  sample ~io:(View.scale 1.6 io) ?font ~at:(Point.v 10. 40.) "scaled x1.6";
  sample ~io:(View.scale 0.75 io) ?font ~at:(Point.v 20. 180.) "scaled x0.75";
  sample
    ~io:(View.rotate 0.3 io)
    ?font ~at:(Point.v 220. 60.) "rotated 0.3 rad";
  sample
    ~io:(io |> View.rotate 0.35 |> View.scale 1.4)
    ?font ~at:(Point.v 160. 120.) "rotated + scaled";
  sample
    ~io:(io |> View.translate (Vec.v 100. 520.) |> View.rotate (-0.5)
       |> View.scale 2.)
    ?font ~at:Point.zero "translate rotate scale"
