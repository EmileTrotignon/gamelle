open Gamelle

(* Texture screenshot scene: pre-render sprites into [Texture.t]s, then draw them
   seven ways — plain, under a rotated view, zoomed in, zoomed out, layered against
   out-of-texture shapes, clipped from the inside, and wiped-then-redrawn — so the
   raylib (render-to-FBO) and browser (offscreen-canvas) implementations can be
   compared.

   The sprite's own contents exercise the drawing paths that must work *inside* a
   texture: nested z-indices (which must collapse into the flat texture identically
   on both backends — the blue disc, at a higher z than the green one, stays on top
   regardless of draw order) and the view transforms (the red bar is drawn under an
   internal rotate, the green disc under an internal translate, the blue disc under
   an internal translate+scale). Cell 4 checks that once drawn the whole texture
   occupies the single outer z-index passed to [Texture.draw]: a shape at a higher
   z covers it, one at a lower z hides behind it. Cell 5 checks that clipping
   *inside* the render function works: the raylib render texture is given a stencil
   buffer (see [Texture.attach_stencil]), without which a concave [View.clip_polygon]
   would collapse to its bounding box. Cell 6 checks {!Texture.wipe}: a texture is
   filled opaque, wiped, then drawn onto again — only the final content shows. *)

let cell_w = 300.
let cell_h = 300.
let w = cell_w *. 7.
let h = cell_h
let cell col = Box.v (Point.v (float col *. cell_w) 0.) (Size.v cell_w cell_h)
let tex_size = 200.
let pi = 4.0 *. atan 1.0

(* The shared sprite contents, drawn through [io] (so a caller can pre-clip it).
   Nested z-indices exercise the internal-z collapse. *)
let paint ~io =
  Box.fill ~io ~color:Color.(rgb 60 60 70) (Window.box ~io);
  (* Internal rotate: a red bar rotated about a point near the top. It also marks
     orientation, so an accidental flip is obvious (z 2). *)
  let rot = io |> View.translate (Vec.v 100. 50.) |> View.rotate 0.5 in
  Box.fill ~io:(View.z_index 2 rot) ~color:Color.red
    (Box.v (Point.v (-.80.) (-.15.)) (Size.v 160. 30.));
  (* Internal z-order plus internal translate and zoom: the green disc is placed
     with a translate (z 1), the blue disc is scaled 1.4x about its centre with a
     translate (z 4). Blue stays over green even though green is drawn after, and
     the two transforms must land pixel-identically on both backends. *)
  let green_io = View.translate (Vec.v 40. 0.) io in
  Circle.fill ~io:(View.z_index 1 green_io) ~color:Color.green
    (Circle.v (Point.v 82. 132.) 40.);
  let blue_io = io |> View.translate (Vec.v 78. 132.) |> View.scale 1.4 in
  Circle.fill ~io:(View.z_index 4 blue_io) ~color:Color.blue
    (Circle.v Point.zero 30.);
  Text.draw ~io:(View.z_index 6 io) ~color:Color.white ~size:20
    ~at:(Point.v 6. 2.) "TL"

(* A concave star, centred in the texture, used as an inside clip. *)
let star =
  let cx = 100. and cy = 100. and outer = 96. and inner = 40. in
  Polygon.v
    (List.init 10 (fun i ->
         let r = if i mod 2 = 0 then outer else inner in
         let a = (float i /. 10.0 *. 2.0 *. pi) -. (pi /. 2.0) in
         Point.v (cx +. (r *. cos a)) (cy +. (r *. sin a))))

(* Two textures, reused by every cell. They are painted once, inside the loop
   (see [paint_textures]); [Window.box ~io] inside the callback reports the
   texture's own size. *)
let sprite = Texture.create (Size.v tex_size tex_size)
let clipped_sprite = Texture.create (Size.v tex_size tex_size)
let wiped_sprite = Texture.create (Size.v tex_size tex_size)
let painted = ref false

(* Paint the textures once, on the first frame (drawing onto a texture needs the
   graphics context, so it happens inside the loop rather than at top level). *)
let paint_textures () =
  if not !painted then begin
    Texture.draw_on_texture sprite paint;
    Texture.draw_on_texture clipped_sprite (fun ~io ->
        paint ~io:(View.clip_polygon star io));
    (* wiped_sprite: fill it opaque, then wipe it, then draw the real content.
       Since drawing only composites, the opaque fill would remain visible if
       [wipe] did not truly clear the buffer to transparent. *)
    Texture.draw_on_texture wiped_sprite (fun ~io ->
        Box.fill ~io ~color:Color.blue (Window.box ~io));
    Texture.wipe wiped_sprite;
    Texture.draw_on_texture wiped_sprite (fun ~io ->
        Circle.fill ~io ~color:Color.red (Circle.v (Point.v 100. 100.) 60.));
    painted := true
  end

let draw_tex ~io b tex =
  let at =
    Point.v
      (Box.x_middle b -. (tex_size /. 2.))
      (Box.y_middle b -. (tex_size /. 2.))
  in
  Texture.draw_on_screen ~io ~at tex

let label ~io b txt =
  Text.draw ~io ~color:Color.white ~size:16
    ~at:(Point.v (Box.x_left b +. 6.) (Box.y_top b +. 6.))
    txt

(* Draw the sprite in cell [col] with the view scaled by [factor] about the cell
   centre (a real zoom), clipped to the cell so it stays contained. *)
let zoom_cell ~io col ~factor txt =
  let b = cell col in
  label ~io b txt;
  let c = Box.center b in
  let k = (1.0 -. factor) /. factor in
  let zio =
    io |> View.clip b |> View.scale factor
    |> View.translate (Vec.v (k *. Point.x c) (k *. Point.y c))
  in
  draw_tex ~io:zio b sprite

let () =
  run () @@ fun ~io () ->
  if Input.is_pressed ~io `escape then raise Exit;
  let io = View.drawing_box (Box.v Point.zero (Size.v w h)) io in
  Box.fill ~io ~color:Color.(rgb 30 30 30) (Window.box ~io);
  paint_textures ();

  (* Cell 0: drawn plainly at native size. *)
  let b = cell 0 in
  label ~io b "Texture.draw";
  draw_tex ~io b sprite;

  (* Cell 1: the view is rotated about the cell centre, so the sprite tilts in
     place — the texture must follow the view rotation like a bitmap. *)
  let b = cell 1 in
  label ~io b "+ rotate";
  let angle = 0.4 in
  let center = Box.center b in
  let rot_io =
    let c' = Point.rotate_around ~center:Point.zero (-.angle) center in
    let d = Vec.(c' - center) in
    io |> View.rotate angle |> View.translate d
  in
  draw_tex ~io:rot_io b sprite;

  (* Cell 2: zoomed in 3x — the fixed-size raster is sampled up, like a bitmap. *)
  zoom_cell ~io 2 ~factor:3.0 "+ zoom 3x";

  (* Cell 3: zoomed out 3x — the minification counterpart. *)
  zoom_cell ~io 3 ~factor:(1.0 /. 3.0) "+ zoom /3";

  (* Cell 4: outer z-index. The sprite occupies the single z passed to
     [Texture.draw_on_screen]; the magenta disc at a higher z covers it, the gold
     disc at a lower z hides behind it. *)
  let b = cell 4 in
  label ~io b "+ outer z";
  let c = Box.center b in
  Circle.fill ~io:(View.z_index (-1) io) ~color:Color.gold
    (Circle.v Vec.(c + Vec.v (-.60.) (-.40.)) 55.);
  draw_tex ~io:(View.z_index 0 io) b sprite;
  Circle.fill ~io:(View.z_index 5 io) ~color:Color.magenta
    (Circle.v Vec.(c + Vec.v 60. 40.) 55.);

  (* Cell 5: the sprite is clipped from the inside to a concave star before being
     drawn — only the star's points show the sprite, the rest is transparent. *)
  let b = cell 5 in
  label ~io b "+ inner clip";
  draw_tex ~io b clipped_sprite;

  (* Cell 6: [wiped_sprite] was filled opaque, wiped, then drawn onto again, so
     only the final red disc shows on transparent — no trace of the wiped fill. *)
  let b = cell 6 in
  label ~io b "+ wipe";
  draw_tex ~io b wiped_sprite
