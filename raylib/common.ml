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
   [View.clip] / [View.clip_polygon] was applied. raylib only offers an
   axis-aligned scissor, so to clip against the (possibly rotated, possibly
   concave) polygon we render the draw into an offscreen texture, then composite
   it back through a shader that keeps each fragment by its signed distance to
   the polygon — antialiasing the boundary for convex and concave shapes alike.
   A screen-sized texture is reused for every clipped draw, and everything
   already funnels through [with_scissor], so this covers all primitives
   (shapes, textures, text) uniformly, per draw call — which keeps each draw's
   own clip and the z-order (draws run sorted, one at a time) intact. *)

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

(* The clip polygon is passed as its edges [clipEdges[i] = (ax, ay, bx, by)] (up
   to [max_clip_edges] of them). Per fragment the shader takes the signed
   distance to the polygon — the distance to the nearest edge, made positive
   inside via an even-odd ray-crossing test — and softens it into a coverage
   value, which antialiases the boundary for convex and concave polygons alike. *)
let max_clip_edges = 256

let clip_fs =
  Printf.sprintf
    {glsl|
#version 330
precision mediump float;
in vec2 fragTexCoord;
in vec4 fragColor;
uniform sampler2D texture0;
uniform vec4 clipEdges[%d];
uniform int clipCount;
uniform float screenHeight;
out vec4 finalColor;
float segDist(vec2 p, vec2 a, vec2 b) {
    vec2 pa = p - a;
    vec2 ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - (ba * h));
}
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float dist = 1e20;
    bool inside = false;
    for (int i = 0; i < clipCount; i++) {
        vec2 a = clipEdges[i].xy;
        vec2 b = clipEdges[i].zw;
        dist = min(dist, segDist(pos, a, b));
        // even-odd ray crossing (a horizontal ray to +x)
        if ((a.y > pos.y) != (b.y > pos.y)) {
            float xint = a.x + (((pos.y - a.y) / (b.y - a.y)) * (b.x - a.x));
            if (pos.x < xint) inside = !inside;
        }
    }
    float cov = smoothstep(-0.75, 0.75, inside ? dist : -dist);
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
let clip_buf_edges = Ctypes.CArray.make Ctypes.float (4 * max_clip_edges)

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
          (* Composite it back through the clip shader (signed-distance coverage
             against the polygon edges) with a premultiplied-alpha blend. *)
          let s = get_clip_shader () in
          let np = Array.length pts in
          let n = min np max_clip_edges in
          (* Emit the [n] edges of the *closed* polygon (vertex i -> vertex
             (i+1) mod n). When the polygon fits within the cap [n = np] and
             every vertex is used exactly; otherwise the vertices are evenly
             subsampled. Closing over [mod n] is what matters: truncating to an
             open edge list (as a naive [mod np] over a shortened loop would)
             leaves the shader's even-odd fill without a boundary on one side,
             so the clip leaks — this is the bug that broke oedipus's
             many-vertex visibility polygons. *)
          let vx k = if n = np then pts.(k) else pts.(k * np / n) in
          for i = 0 to n - 1 do
            let ax, ay = vx i and bx', by' = vx ((i + 1) mod n) in
            Ctypes.CArray.set clip_buf_edges (4 * i) ax;
            Ctypes.CArray.set clip_buf_edges ((4 * i) + 1) ay;
            Ctypes.CArray.set clip_buf_edges ((4 * i) + 2) bx';
            Ctypes.CArray.set clip_buf_edges ((4 * i) + 3) by'
          done;
          Raylib.set_shader_value_v s.shader s.loc_edges
            Ctypes.(CArray.start clip_buf_edges |> to_voidp)
            Raylib.ShaderUniformDataType.Vec4 n;
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
