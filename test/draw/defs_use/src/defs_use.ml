open Gamelle

(* Reproduction scene for the <defs>/<use> SVG bug.

   [assets/defs_use.svg] draws a solid background rect directly and puts all its
   foreground art inside a <defs><g id="art">...</g></defs>, instantiated with a
   single <use xlink:href="#art">. This is exactly how the SWF->SVG exporter
   (FFDec) emits every layer of an animation.

   The browser backend hands the raw SVG to the browser, which resolves <use> and
   draws the art on top of the background. The raylib backend rasterises via
   nanosvg, which does not resolve <use>/<defs>, so only the background survives.
   Side by side, the raylib cell shows a plain blue square while the browser cell
   shows the blue square with the yellow disc, red triangle and white dot on top
   — that visible gap is the bug. *)

let w = 320.
let h = 320.

let () =
  run () @@ fun ~io () ->
  if Input.is_pressed ~io `escape then raise Exit;
  let io = View.drawing_box (Box.v Point.zero (Size.v w h)) io in
  Box.fill ~io ~color:Color.(rgb 40 40 40) (Window.box ~io);
  Text.draw ~io ~color:Color.white ~size:16 ~at:(Point.v 6. 6.) "defs/use svg";
  let sw = float (Svg.width Assets.defs_use)
  and sh = float (Svg.height Assets.defs_use) in
  let at = Point.v ((w -. sw) /. 2.0) ((h -. sh) /. 2.0) in
  Svg.draw ~io ~at Assets.defs_use
