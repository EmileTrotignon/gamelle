module Delayed = Gamelle_common.Delayed
module Transform = Gamelle_common.Transform
open Gamelle_common
open Geometry

(* --- Raw OpenGL stencil entry points ---

   raylib/rlgl expose no stencil control, so we bind the (GL 1.0/2.0 core)
   stencil functions directly. They resolve from the already-loaded libGL — the
   process has a live GL context (raylib created it) — via the default dynamic
   symbol table, so no explicit [Dl.dlopen] is needed. Used by [with_scissor] to
   clip against arbitrary polygons through the stencil buffer (see there). *)
module Gl = struct
  open Ctypes
  open Foreign

  let enable = foreign "glEnable" (int @-> returning void)
  let disable = foreign "glDisable" (int @-> returning void)
  let stencil_mask = foreign "glStencilMask" (int @-> returning void)
  let stencil_func = foreign "glStencilFunc" (int @-> int @-> int @-> returning void)
  let stencil_op = foreign "glStencilOp" (int @-> int @-> int @-> returning void)

  let stencil_op_separate =
    foreign "glStencilOpSeparate" (int @-> int @-> int @-> int @-> returning void)

  let clear = foreign "glClear" (int @-> returning void)
  let clear_stencil = foreign "glClearStencil" (int @-> returning void)

  (* GLenum / bitfield constants. *)
  let stencil_test = 0x0B90
  let stencil_buffer_bit = 0x0400
  let always = 0x0207
  let equal = 0x0202
  let notequal = 0x0205
  let keep = 0x1E00
  let replace = 0x1E01
  let incr_wrap = 0x8507
  let decr_wrap = 0x8508
  let front = 0x0404
  let back = 0x0405
end

let v2 x y = Raylib.Vector2.create x y

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

   The clip region ([io.clip]) is a list of screen-space polygons, each frozen
   when its [View.clip] / [View.clip_polygon] was applied. Successive clips only
   shrink the visible zone, so a pixel survives only where it lies inside every
   polygon. raylib only offers an axis-aligned scissor, so to clip against the
   (possibly rotated, possibly concave) polygons we render the draw into an
   offscreen texture, then composite it back through a coverage mask that is the
   product of each polygon's coverage — antialiasing the boundary for convex and
   concave shapes alike.
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

(* The clip polygon is passed as its edges in a texture, one edge [(ax, ay, bx,
   by)] per RGBA32F texel (see [set_clip_edges]) — a texture rather than a
   uniform array so any number of edges fits, matching the browser, which draws
   every vertex (a subsampled uniform array would drop corners and distort the
   boundary; oedipus's visibility polygons run to several hundred vertices). Per
   fragment the shader takes the signed distance to the polygon — the distance to
   the nearest edge, made positive inside via a non-zero winding test — and
   softens it into a coverage value, antialiasing the boundary. The whole loop
   runs once per clip region (the coverage is cached in a mask), not per draw. *)
let clip_fs =
  {glsl|
#version 330
precision mediump float;
in vec2 fragTexCoord;
in vec4 fragColor;
uniform sampler2D texture0;
uniform sampler2D clipEdgesTex;
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
    // Non-zero winding number (matching the browser's canvas clip, whose
    // default fill rule is non-zero): a horizontal ray to +x, each crossing
    // counted +1 upward / -1 downward via the isLeft sign. An even-odd test
    // would instead cancel self-overlapping regions (a visibility polygon's
    // crossing rays, a pentagram's centre) into holes.
    int wind = 0;
    for (int i = 0; i < clipCount; i++) {
        vec4 e = texelFetch(clipEdgesTex, ivec2(i, 0), 0);
        vec2 a = e.xy;
        vec2 b = e.zw;
        dist = min(dist, segDist(pos, a, b));
        float side = ((b.x - a.x) * (pos.y - a.y)) - ((pos.x - a.x) * (b.y - a.y));
        if (a.y <= pos.y) {
            if (b.y > pos.y && side > 0.0) wind++;
        } else {
            if (b.y <= pos.y && side < 0.0) wind--;
        }
    }
    bool inside = wind != 0;
    float cov = smoothstep(-0.75, 0.75, inside ? dist : -dist);
    // The texture holds premultiplied colour (drawn onto transparent black),
    // and is composited with a premultiplied blend, so scale all four channels
    // by the coverage.
    vec4 texel = texture(texture0, fragTexCoord) * fragColor;
    finalColor = texel * cov;
}
|glsl}

type clip_shader = {
  shader : Raylib.Shader.t;
  loc_edges_tex : Raylib.ShaderLoc.t;
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
          loc_edges_tex = loc "clipEdgesTex";
          loc_count = loc "clipCount";
          loc_screen_height = loc "screenHeight";
        }
      in
      clip_shader := Some s;
      s

let clip_buf1 = Ctypes.CArray.make Ctypes.float 1
let clip_buf_int = Ctypes.CArray.make Ctypes.int32_t 1

(* The clip edges, uploaded to a 1-row RGBA32F texture ([clipEdgesTex] above):
   one edge per texel, [(ax, ay)] in RG and [(bx, by)] in BA. Grown on demand so
   any vertex count fits. *)
let clip_edges_format = Raylib.PixelFormat.Uncompressed_r32g32b32a32
let clip_edges_cap = ref 0
let clip_edges_buf = ref (Ctypes.CArray.make Ctypes.float 0)
let clip_edges_tex = ref (None : Raylib.Texture.t option)

let ensure_edge_capacity n =
  if n > !clip_edges_cap then begin
    (match !clip_edges_tex with
    | Some t -> Raylib.Rlgl.unload_texture (Raylib.Texture.id t)
    | None -> ());
    let buf = Ctypes.CArray.make Ctypes.float (4 * n) in
    let id =
      Raylib.Rlgl.load_texture
        Ctypes.(CArray.start buf |> to_voidp)
        n 1 clip_edges_format 1
    in
    clip_edges_buf := buf;
    clip_edges_tex := Some (Raylib.Texture.create id n 1 1 clip_edges_format);
    clip_edges_cap := n
  end

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

(* Two screen-sized offscreen layers, reused across draws and reallocated only
   when the screen size changes: [scratch_rt] receives each clipped draw, and
   [mask_rt] holds the current clip polygon's antialiased coverage. *)
let scratch_rt : Raylib.RenderTexture.t option ref = ref None
let mask_rt : Raylib.RenderTexture.t option ref = ref None

let get_rt slot =
  let w = Raylib.get_screen_width () and h = Raylib.get_screen_height () in
  let matches rt =
    let t = Raylib.RenderTexture.texture rt in
    Raylib.Texture.width t = w && Raylib.Texture.height t = h
  in
  match !slot with
  | Some rt when matches rt -> rt
  | prev ->
      Option.iter Raylib.unload_render_texture prev;
      let rt = Raylib.load_render_texture w h in
      slot := Some rt;
      rt

(* Upload the clip polygon's [np] closed edges (vertex i -> vertex (i+1) mod np)
   into the edge texture and point the shader at it. Every vertex is kept — no
   cap, no subsampling — so the boundary matches the browser's exactly. Closing
   over [mod np] keeps the edge loop closed, without which the winding fill would
   have no boundary on one side and leak. *)
let set_clip_edges s pts =
  let np = Array.length pts in
  ensure_edge_capacity np;
  let buf = !clip_edges_buf in
  for i = 0 to np - 1 do
    let ax, ay = pts.(i) and bx', by' = pts.((i + 1) mod np) in
    Ctypes.CArray.set buf (4 * i) ax;
    Ctypes.CArray.set buf ((4 * i) + 1) ay;
    Ctypes.CArray.set buf ((4 * i) + 2) bx';
    Ctypes.CArray.set buf ((4 * i) + 3) by'
  done;
  let tex = Option.get !clip_edges_tex in
  Raylib.Rlgl.update_texture (Raylib.Texture.id tex) 0 0 np 1 clip_edges_format
    Ctypes.(CArray.start buf |> to_voidp);
  Raylib.set_shader_value_texture s.shader s.loc_edges_tex tex;
  set_int s.shader s.loc_count np;
  set_float s.shader s.loc_screen_height (float (Raylib.get_render_height ()))

(* The clip polygons whose combined coverage currently sits in [mask_rt], and
   the screen size it was built for. A whole clip region reuses one [io.clip]
   list for all its draws, so keying the cached mask on that list's identity
   runs the (per-pixel, per-edge) coverage pass once per region rather than once
   per draw — which is what made oedipus's many-vertex visibility clip slow. *)
let mask_polys : Geometry.Polygon.t list ref = ref []
let mask_size : (int * int) ref = ref (0, 0)

(* Render the intersection of [polys]'s antialiased coverage into [mask_rt] over
   the (already intersected) bounding box [bx,by,bw,bh], memoised on the list's
   identity and the screen size. The signed-distance shader (its per-fragment
   loop over one polygon's edges) runs once per polygon here. Each [pts] is that
   polygon's vertices as a float tuple array. *)
let get_clip_mask polys pts_list bx by bw bh =
  let sw = Raylib.get_screen_width () and sh = Raylib.get_screen_height () in
  let cached = !mask_polys == polys && !mask_size = (sw, sh) in
  let mask = get_rt mask_rt in
  if not cached then begin
    let s = get_clip_shader () in
    Raylib.begin_texture_mode mask;
    Raylib.begin_scissor_mode bx by bw bh;
    (* Start from full coverage, then multiply each polygon's coverage in, so
       the mask ends up as the product — a pixel kept only where every polygon
       keeps it. *)
    Raylib.clear_background Raylib.Color.white;
    List.iter
      (fun pts ->
        (* Multiply the shader's (cov,cov,cov,cov) output into the mask: source
           factor [dst_color] and destination factor [zero] give
           [coverage * mask] per channel. *)
        Raylib.Rlgl.set_blend_factors Raylib.Rlgl.BlendFactor.dst_color
          Raylib.Rlgl.BlendFactor.zero Raylib.Rlgl.BlendFunction.func_add;
        Raylib.begin_blend_mode Raylib.BlendMode.Custom;
        Raylib.begin_shader_mode s.shader;
        (* Bind the edge texture and set the uniforms only after the shader is
           active: entering shader mode flushes the batch and clears raylib's
           registered texture units, so a sampler bound before it would be lost. *)
        set_clip_edges s pts;
        Raylib.draw_rectangle bx by bw bh Raylib.Color.white;
        Raylib.end_shader_mode ();
        Raylib.end_blend_mode ())
      pts_list;
    Raylib.end_scissor_mode ();
    Raylib.end_texture_mode ();
    mask_polys := polys;
    mask_size := (sw, sh)
  end;
  mask

(* Draw a render texture's bbox region back at the same screen location. The
   render texture is stored bottom-up, so the source height is negated to flip
   it into screen orientation; the mask (also a render texture) shares that
   orientation, so multiplying it into [scratch_rt] uses the same mapping. *)
let draw_rt_bbox t bx by bw bh sh =
  Raylib.draw_texture_rec t
    (Raylib.Rectangle.create (float bx)
       (float (sh - by - bh))
       (float bw)
       (-.float bh))
    (Raylib.Vector2.create (float bx) (float by))
    Raylib.Color.white

(* The intersection of the clip polygons' (clamped) bounding boxes, as
   [(bx,by,bw,bh)] screen pixels, or [None] when empty: a pixel outside any one
   polygon's box is clipped away, so only their common box can hold visible
   pixels. Bounding the work to this box keeps every clip pass local. *)
let clip_bbox pts_list sw sh =
  let minx = ref 0. and miny = ref 0. in
  let maxx = ref (float sw) and maxy = ref (float sh) in
  List.iter
    (fun pts ->
      let pminx = ref infinity and pminy = ref infinity in
      let pmaxx = ref neg_infinity and pmaxy = ref neg_infinity in
      Array.iter
        (fun (x, y) ->
          pminx := Float.min !pminx x;
          pminy := Float.min !pminy y;
          pmaxx := Float.max !pmaxx x;
          pmaxy := Float.max !pmaxy y)
        pts;
      minx := Float.max !minx !pminx;
      miny := Float.max !miny !pminy;
      maxx := Float.min !maxx !pmaxx;
      maxy := Float.min !maxy !pmaxy)
    pts_list;
  let bx = int_of_float (Float.max 0. !minx) in
  let by = int_of_float (Float.max 0. !miny) in
  let bw = min (int_of_float (Float.min !maxx (float sw)) + 2 - bx) (sw - bx) in
  let bh = min (int_of_float (Float.min !maxy (float sh)) + 2 - by) (sh - by) in
  if bw > 0 && bh > 0 then Some (bx, by, bw, bh) else None

(* --- Fallback: the offscreen signed-distance mask (retained for clip stacks
   deeper than the stencil path handles). Renders the draw into an offscreen
   layer, multiplies in the memoised coverage mask, and composites back. --- *)
let with_scissor_mask ~io f pts_list bx by bw bh =
  let sh = Raylib.get_screen_height () in
  let mask = get_clip_mask io.clip pts_list bx by bw bh in
  let rt = get_rt scratch_rt in
  Raylib.begin_texture_mode rt;
  Raylib.begin_scissor_mode bx by bw bh;
  Raylib.clear_background Raylib.Color.blank;
  Raylib.Rlgl.set_blend_factors_separate Raylib.Rlgl.BlendFactor.src_alpha
    Raylib.Rlgl.BlendFactor.one_minus_src_alpha Raylib.Rlgl.BlendFactor.one
    Raylib.Rlgl.BlendFactor.one_minus_src_alpha Raylib.Rlgl.BlendFunction.func_add
    Raylib.Rlgl.BlendFunction.func_add;
  Raylib.begin_blend_mode Raylib.BlendMode.Custom_separate;
  f ();
  Raylib.end_blend_mode ();
  Raylib.Rlgl.set_blend_factors Raylib.Rlgl.BlendFactor.zero
    Raylib.Rlgl.BlendFactor.src_color Raylib.Rlgl.BlendFunction.func_add;
  Raylib.begin_blend_mode Raylib.BlendMode.Custom;
  draw_rt_bbox (Raylib.RenderTexture.texture mask) bx by bw bh sh;
  Raylib.end_blend_mode ();
  Raylib.end_scissor_mode ();
  Raylib.end_texture_mode ();
  Raylib.begin_blend_mode Raylib.BlendMode.Alpha_premultiply;
  draw_rt_bbox (Raylib.RenderTexture.texture rt) bx by bw bh sh;
  Raylib.end_blend_mode ()

(* --- Stencil clip path (fast, the default) ---

   Instead of compositing every draw through a per-fragment edge-loop shader (a
   full offscreen round-trip per draw, whose mask costs O(pixels x edges) — the
   bottleneck on oedipus's ~500-vertex visibility polygon), build the clip
   region once into the GPU stencil buffer, then draw primitives normally with
   the stencil test rejecting everything outside. This is the raylib analogue of
   the browser backend's native [C.clip]: establish coverage once, apply it
   cheaply. Each clip polygon's non-zero-winding interior is stamped into its own
   result bit; a pixel passes only where every used result bit is set. MSAA makes
   the per-sample stencil an antialiased boundary, matching the browser. *)

(* Which clip stack currently lives in the stencil buffer (by list identity and
   screen size): rebuilt only when the active region changes, like [mask_polys]. *)
let stencil_polys : Geometry.Polygon.t list ref = ref []
let stencil_size : (int * int) ref = ref (0, 0)

(* Low [scratch_mask] bits are winding scratch, shared across polygons; each
   polygon then owns one "inside" result bit. Up to 5 polygons (bits 3..7);
   deeper stacks fall back to the mask path. *)
let scratch_mask = 0x07
let result_bit i = 1 lsl (3 + i)
let max_stencil_polys = 5

let build_stencil pts_list bx by bw bh =
  Raylib.Rlgl.draw_render_batch_active ();
  Raylib.begin_scissor_mode bx by bw bh;
  Raylib.Rlgl.color_mask false false false false;
  (* Two-sided winding needs both faces rasterised, so culling must be off. *)
  Raylib.Rlgl.disable_backface_culling ();
  Gl.enable Gl.stencil_test;
  Gl.stencil_mask 0xFF;
  Gl.clear_stencil 0;
  Gl.clear Gl.stencil_buffer_bit;
  List.iteri
    (fun i pts ->
      let rb = result_bit i in
      (* Non-zero winding of the polygon into the scratch bits: a triangle fan
         with front faces incrementing and back faces decrementing, so concave
         and self-overlapping polygons (a pentagram, a visibility blob whose rays
         cross) come out right — the browser's non-zero canvas-clip rule. *)
      Gl.stencil_mask scratch_mask;
      Gl.stencil_func Gl.always 0 0xFF;
      Gl.stencil_op_separate Gl.front Gl.keep Gl.keep Gl.incr_wrap;
      Gl.stencil_op_separate Gl.back Gl.keep Gl.keep Gl.decr_wrap;
      let n = Array.length pts in
      let x0, y0 = pts.(0) in
      let v0 = v2 x0 y0 in
      for k = 1 to n - 2 do
        let xa, ya = pts.(k) and xb, yb = pts.(k + 1) in
        Raylib.draw_triangle v0 (v2 xa ya) (v2 xb yb) Raylib.Color.white
      done;
      Raylib.Rlgl.draw_render_batch_active ();
      (* Stamp [rb] where the winding is non-zero. The func ref [rb] carries no
         scratch bits, so under [scratch_mask] the NOTEQUAL test reads as
         "scratch <> 0", and REPLACE writes [rb & stencil_mask = rb]. *)
      Gl.stencil_mask rb;
      Gl.stencil_func Gl.notequal rb scratch_mask;
      Gl.stencil_op Gl.keep Gl.keep Gl.replace;
      Raylib.draw_rectangle bx by bw bh Raylib.Color.white;
      Raylib.Rlgl.draw_render_batch_active ();
      (* Reset the scratch bits before the next polygon (result bits kept). *)
      Gl.stencil_mask scratch_mask;
      Gl.clear Gl.stencil_buffer_bit)
    pts_list;
  Raylib.Rlgl.color_mask true true true true;
  Raylib.Rlgl.enable_backface_culling ();
  Gl.disable Gl.stencil_test;
  Gl.stencil_mask 0xFF;
  Raylib.end_scissor_mode ()

let with_scissor ~io f =
  match io.clip with
  | [] -> f ()
  | polys ->
      let pts_list =
        List.map
          (fun poly ->
            Array.of_list (List.map Vec.to_tuple (Polygon.points poly)))
          polys
      in
      (* A region with fewer than 3 vertices is degenerate and clips everything
         away, so the intersection is empty and nothing is drawn. *)
      if List.exists (fun pts -> Array.length pts < 3) pts_list then ()
      else begin
        let sw = Raylib.get_screen_width ()
        and sh = Raylib.get_screen_height () in
        match clip_bbox pts_list sw sh with
        | None -> ()
        | Some (bx, by, bw, bh) ->
            if List.length polys > max_stencil_polys then
              with_scissor_mask ~io f pts_list bx by bw bh
            else begin
              (* Rebuild the stencil only when the active region changes. *)
              if not (!stencil_polys == polys && !stencil_size = (sw, sh)) then begin
                build_stencil pts_list bx by bw bh;
                stencil_polys := polys;
                stencil_size := (sw, sh)
              end;
              let all =
                List.fold_left
                  (fun a i -> a lor result_bit i)
                  0
                  (List.init (List.length polys) Fun.id)
              in
              (* Draw the primitive normally, kept only where every result bit is
                 set (i.e. inside every clip polygon). *)
              Gl.enable Gl.stencil_test;
              Gl.stencil_mask 0x00;
              Gl.stencil_func Gl.equal all all;
              Gl.stencil_op Gl.keep Gl.keep Gl.keep;
              Raylib.begin_scissor_mode bx by bw bh;
              f ();
              Raylib.Rlgl.draw_render_batch_active ();
              Raylib.end_scissor_mode ();
              Gl.disable Gl.stencil_test;
              Gl.stencil_mask 0xFF
            end
      end
