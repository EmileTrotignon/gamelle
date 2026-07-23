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

(* --- Polygon clipping ---

   The clip region ([io.clip]) is a screen-space polygon, frozen when
   [View.clip] was applied — always a convex quadrilateral, since it is a box
   projected through the (affine) view. raylib only offers an axis-aligned
   scissor, so to clip against the rotated quad exactly we render the draw into
   an offscreen texture, then composite it back through a shader that fades out
   fragments outside the quad. A single screen-sized texture is reused for every
   clipped draw, and everything already funnels through [with_scissor], so this
   covers all primitives (shapes, textures, text) uniformly, per draw call —
   which keeps each draw's own clip and the z-order (draws run sorted, one at a
   time) intact. *)

let clip_vs =
  {glsl|
#version 330
in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;
uniform mat4 mvp;
out vec2 fragTexCoord;
out vec4 fragColor;
void main() {
    fragTexCoord = vertexTexCoord;
    fragColor = vertexColor;
    gl_Position = mvp * vec4(vertexPosition, 1.0);
}
|glsl}

(* [clipN{i}]/[clipD{i}] are the four inward-facing edge half-planes of the clip
   quad (normalised, so the dot product is a signed pixel distance). A fragment's
   coverage is the softened distance to the nearest edge, clamped to the interior
   — antialiasing the rotated clip boundary. *)
let clip_fs =
  {glsl|
#version 330
precision mediump float;
in vec2 fragTexCoord;
in vec4 fragColor;
uniform sampler2D texture0;
uniform vec2 clipN0; uniform float clipD0;
uniform vec2 clipN1; uniform float clipD1;
uniform vec2 clipN2; uniform float clipD2;
uniform vec2 clipN3; uniform float clipD3;
uniform float screenHeight;
out vec4 finalColor;
float edge(vec2 pos, vec2 n, float d) {
    return smoothstep(-0.75, 0.75, dot(pos, n) - d);
}
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float cov = edge(pos, clipN0, clipD0);
    cov = min(cov, edge(pos, clipN1, clipD1));
    cov = min(cov, edge(pos, clipN2, clipD2));
    cov = min(cov, edge(pos, clipN3, clipD3));
    vec4 texel = texture(texture0, fragTexCoord) * fragColor;
    finalColor = vec4(texel.rgb, texel.a * cov);
}
|glsl}

type clip_shader = {
  shader : Raylib.Shader.t;
  loc_n : Raylib.ShaderLoc.t array;
  loc_d : Raylib.ShaderLoc.t array;
  loc_screen_height : Raylib.ShaderLoc.t;
}

let clip_shader : clip_shader option ref = ref None

let get_clip_shader () =
  match !clip_shader with
  | Some s -> s
  | None ->
      let shader = Raylib.load_shader_from_memory clip_vs clip_fs in
      let loc name = Raylib.get_shader_location shader name in
      let s =
        {
          shader;
          loc_n = Array.init 4 (fun i -> loc (Printf.sprintf "clipN%d" i));
          loc_d = Array.init 4 (fun i -> loc (Printf.sprintf "clipD%d" i));
          loc_screen_height = loc "screenHeight";
        }
      in
      clip_shader := Some s;
      s

let clip_buf1 = Ctypes.CArray.make Ctypes.float 1
let clip_buf2 = Ctypes.CArray.make Ctypes.float 2

let set_float shader loc v =
  Ctypes.CArray.set clip_buf1 0 v;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start clip_buf1 |> to_voidp)
    Raylib.ShaderUniformDataType.Float

let set_vec2 shader loc x y =
  Ctypes.CArray.set clip_buf2 0 x;
  Ctypes.CArray.set clip_buf2 1 y;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start clip_buf2 |> to_voidp)
    Raylib.ShaderUniformDataType.Vec2

(* The screen-sized offscreen layer clipped draws are rendered into. Reused
   across draws and reallocated only when the screen size changes. *)
let scratch_rt : Raylib.RenderTexture.t option ref = ref None

let get_scratch_rt () =
  let w = Raylib.get_screen_width () and h = Raylib.get_screen_height () in
  let matches rt =
    let t = Raylib.RenderTexture.texture rt in
    Raylib.Texture.width t = w && Raylib.Texture.height t = h
  in
  match !scratch_rt with
  | Some rt when matches rt -> rt
  | prev ->
      Option.iter Raylib.unload_render_texture prev;
      let rt = Raylib.load_render_texture w h in
      scratch_rt := Some rt;
      rt

(* Inward half-planes of the (convex) clip quad, as normalised [(normal, d)]
   with [dot(p, normal) >= d] inside. The normal is oriented towards the quad's
   centroid. *)
let clip_half_planes pts =
  let n = Array.length pts in
  let cx = ref 0. and cy = ref 0. in
  Array.iter
    (fun (x, y) ->
      cx := !cx +. x;
      cy := !cy +. y)
    pts;
  let cx = !cx /. float n and cy = !cy /. float n in
  Array.init n (fun i ->
      let ax, ay = pts.(i) and bx, by = pts.((i + 1) mod n) in
      let nx = -.(by -. ay) and ny = bx -. ax in
      let len = Float.hypot nx ny in
      let nx, ny = if len > 0. then (nx /. len, ny /. len) else (0., 0.) in
      (* Point the normal inwards (towards the centroid). *)
      let s =
        if (nx *. (cx -. ax)) +. (ny *. (cy -. ay)) < 0. then -1. else 1.
      in
      let nx = s *. nx and ny = s *. ny in
      (nx, ny, (nx *. ax) +. (ny *. ay)))

let with_scissor ~io f =
  match io.clip with
  | None -> f ()
  | Some poly ->
      let pts = Array.of_list (List.map Vec.to_tuple (Polygon.points poly)) in
      if Array.length pts < 3 then ()
      else begin
        let sw = Raylib.get_screen_width ()
        and sh = Raylib.get_screen_height () in
        (* Bound the offscreen work to the clip's (clamped) bounding box. *)
        let minx = ref infinity and miny = ref infinity in
        let maxx = ref neg_infinity and maxy = ref neg_infinity in
        Array.iter
          (fun (x, y) ->
            minx := Float.min !minx x;
            miny := Float.min !miny y;
            maxx := Float.max !maxx x;
            maxy := Float.max !maxy y)
          pts;
        let bx = int_of_float (Float.max 0. !minx) in
        let by = int_of_float (Float.max 0. !miny) in
        let bw =
          min (int_of_float (Float.min !maxx (float sw)) + 2 - bx) (sw - bx)
        in
        let bh =
          min (int_of_float (Float.min !maxy (float sh)) + 2 - by) (sh - by)
        in
        if bw > 0 && bh > 0 then begin
          let rt = get_scratch_rt () in
          (* Render the draw into the (bbox-cleared) offscreen layer. *)
          Raylib.begin_texture_mode rt;
          Raylib.begin_scissor_mode bx by bw bh;
          Raylib.clear_background Raylib.Color.blank;
          f ();
          Raylib.end_scissor_mode ();
          Raylib.end_texture_mode ();
          (* Composite it back, faded out beyond the clip quad. *)
          let s = get_clip_shader () in
          let planes = clip_half_planes pts in
          Array.iteri
            (fun i (nx, ny, d) ->
              if i < 4 then begin
                set_vec2 s.shader s.loc_n.(i) nx ny;
                set_float s.shader s.loc_d.(i) d
              end)
            planes;
          (* A degenerate (triangle) quad leaves the 4th plane unset; repeat the
             last real edge so it never rejects extra fragments. *)
          if Array.length planes < 4 then begin
            let nx, ny, d = planes.(Array.length planes - 1) in
            set_vec2 s.shader s.loc_n.(3) nx ny;
            set_float s.shader s.loc_d.(3) d
          end;
          set_float s.shader s.loc_screen_height
            (float (Raylib.get_render_height ()));
          let t = Raylib.RenderTexture.texture rt in
          Raylib.begin_shader_mode s.shader;
          (* The render texture is stored bottom-up, so the source height is
             negated to flip it back to screen orientation. *)
          Raylib.draw_texture_rec t
            (Raylib.Rectangle.create (float bx)
               (float (sh - by - bh))
               (float bw)
               (-.float bh))
            (Raylib.Vector2.create (float bx) (float by))
            Raylib.Color.white;
          Raylib.end_shader_mode ()
        end
      end
