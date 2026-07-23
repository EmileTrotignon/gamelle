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

   The clip region ([io.clip]) is a screen-space convex polygon, frozen when
   [View.clip] / [View.clip_polygon] was applied ([View.clip]'s box projected
   through the affine view is a convex quadrilateral). raylib only offers an
   axis-aligned scissor, so to clip against the rotated polygon exactly we
   render the draw into an offscreen texture, then composite it back through a
   shader that fades out fragments outside the polygon. A screen-sized texture
   is reused for every
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

(* The clip polygon is passed as up to [max_clip_edges] inward-facing edge
   half-planes [clipEdges[i] = (nx, ny, d)] (normalised, so [dot(pos, n) - d] is
   a signed pixel distance), which exactly describe any convex polygon. A
   fragment's coverage is the softened distance to the nearest edge, clamped to
   the interior — antialiasing the (rotated) clip boundary. *)
let max_clip_edges = 16

let clip_fs =
  Printf.sprintf
    {glsl|
#version 330
precision mediump float;
in vec2 fragTexCoord;
in vec4 fragColor;
uniform sampler2D texture0;
uniform vec3 clipEdges[%d];
uniform int clipCount;
uniform float screenHeight;
out vec4 finalColor;
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float cov = 1.0;
    for (int i = 0; i < clipCount; i++) {
        vec3 e = clipEdges[i];
        cov = min(cov, smoothstep(-0.75, 0.75, dot(pos, e.xy) - e.z));
    }
    // The texture holds premultiplied colour (drawn onto transparent black),
    // and is composited with a premultiplied blend, so scale all four channels
    // by the coverage.
    vec4 texel = texture(texture0, fragTexCoord) * fragColor;
    finalColor = texel * cov;
}
|glsl}
    max_clip_edges

type clip_shader = {
  shader : Raylib.Shader.t;
  loc_edges : Raylib.ShaderLoc.t;
  loc_count : Raylib.ShaderLoc.t;
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
          loc_edges = loc "clipEdges";
          loc_count = loc "clipCount";
          loc_screen_height = loc "screenHeight";
        }
      in
      clip_shader := Some s;
      s

let clip_buf1 = Ctypes.CArray.make Ctypes.float 1
let clip_buf_int = Ctypes.CArray.make Ctypes.int32_t 1
let clip_buf_edges = Ctypes.CArray.make Ctypes.float (3 * max_clip_edges)

let set_float shader loc v =
  Ctypes.CArray.set clip_buf1 0 v;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start clip_buf1 |> to_voidp)
    Raylib.ShaderUniformDataType.Float

let set_int shader loc v =
  Ctypes.CArray.set clip_buf_int 0 (Int32.of_int v);
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start clip_buf_int |> to_voidp)
    Raylib.ShaderUniformDataType.Int

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
          (* Render the draw into the (bbox-cleared) offscreen layer. Colour is
             composited normally, but the alpha channel accumulates as coverage
             (src factor [one]) so the layer ends up with premultiplied colour
             over a correct alpha — needed to composite it back without the
             double-darkening a plain alpha blend into transparent would give. *)
          Raylib.begin_texture_mode rt;
          Raylib.begin_scissor_mode bx by bw bh;
          Raylib.clear_background Raylib.Color.blank;
          Raylib.Rlgl.set_blend_factors_separate
            Raylib.Rlgl.BlendFactor.src_alpha
            Raylib.Rlgl.BlendFactor.one_minus_src_alpha
            Raylib.Rlgl.BlendFactor.one
            Raylib.Rlgl.BlendFactor.one_minus_src_alpha
            Raylib.Rlgl.BlendFunction.func_add
            Raylib.Rlgl.BlendFunction.func_add;
          Raylib.begin_blend_mode Raylib.BlendMode.Custom_separate;
          f ();
          Raylib.end_blend_mode ();
          Raylib.end_scissor_mode ();
          Raylib.end_texture_mode ();
          (* Composite it back through the clip shader (which fades out fragments
             beyond the polygon), with a premultiplied-alpha blend. *)
          let s = get_clip_shader () in
          let planes = clip_half_planes pts in
          let n = min (Array.length planes) max_clip_edges in
          Array.iteri
            begin fun i (nx, ny, d) ->
              if i < n then begin
                Ctypes.CArray.set clip_buf_edges (3 * i) nx;
                Ctypes.CArray.set clip_buf_edges ((3 * i) + 1) ny;
                Ctypes.CArray.set clip_buf_edges ((3 * i) + 2) d
              end
            end
            planes;
          Raylib.set_shader_value_v s.shader s.loc_edges
            Ctypes.(CArray.start clip_buf_edges |> to_voidp)
            Raylib.ShaderUniformDataType.Vec3 n;
          set_int s.shader s.loc_count n;
          set_float s.shader s.loc_screen_height
            (float (Raylib.get_render_height ()));
          let t = Raylib.RenderTexture.texture rt in
          Raylib.begin_blend_mode Raylib.BlendMode.Alpha_premultiply;
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
          Raylib.end_shader_mode ();
          Raylib.end_blend_mode ()
        end
      end
