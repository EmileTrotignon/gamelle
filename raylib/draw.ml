open Common
open Gamelle_common
open Geometry
module Delayed = Gamelle_common.Delayed

let get_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  to_raylib_color c

let v2 x y = Raylib.Vector2.create x y
let tau = 8.0 *. atan 1.0

let draw ~io bmp p =
  let bmp = Delayed.force ~io bmp in
  let scale = io.view.Transform.scale in
  let x, y = project ~io p in
  let src =
    Raylib.Rectangle.create bmp.Bitmap.src_x bmp.Bitmap.src_y
      (float bmp.Bitmap.w) (float bmp.Bitmap.h)
  in
  let dst =
    Raylib.Rectangle.create x y
      (float bmp.Bitmap.w *. scale)
      (float bmp.Bitmap.h *. scale)
  in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  with_scissor ~io @@ fun () ->
  Raylib.draw_texture_pro bmp.Bitmap.texture src dst (v2 0. 0.) angle
    Raylib.Color.white

(* [draw_line], [draw_poly] and [draw_rect] are defined after the shader helpers
   below: their straight edges are antialiased with an SDF segment shader so they
   match the browser backend (which strokes with antialiasing). *)

(* Signed area of a polygon (shoelace). Sign indicates winding order. *)
let signed_area pts =
  let n = Array.length pts in
  let a = ref 0. in
  for i = 0 to n - 1 do
    let x0, y0 = pts.(i) in
    let x1, y1 = pts.((i + 1) mod n) in
    a := !a +. ((x0 *. y1) -. (x1 *. y0))
  done;
  !a /. 2.

let point_in_triangle px py (ax, ay) (bx, by) (cx, cy) =
  let d1 = ((px -. bx) *. (ay -. by)) -. ((ax -. bx) *. (py -. by)) in
  let d2 = ((px -. cx) *. (by -. cy)) -. ((bx -. cx) *. (py -. cy)) in
  let d3 = ((px -. ax) *. (cy -. ay)) -. ((cx -. ax) *. (py -. ay)) in
  let has_neg = d1 < 0. || d2 < 0. || d3 < 0. in
  let has_pos = d1 > 0. || d2 > 0. || d3 > 0. in
  not (has_neg && has_pos)

(* Ear-clipping triangulation of a simple polygon (convex or concave). Returns
   a list of triangles as point triples, in the same coordinates as [pts]. *)
let triangulate pts =
  let pts = Array.of_list pts in
  let n = Array.length pts in
  if n < 3 then []
  else begin
    let orient = if signed_area pts >= 0. then 1. else -1. in
    let cross (ax, ay) (bx, by) (cx, cy) =
      ((bx -. ax) *. (cy -. ay)) -. ((cx -. ax) *. (by -. ay))
    in
    let is_ear remaining ip ic inx =
      let a = pts.(ip) and b = pts.(ic) and c = pts.(inx) in
      (* Convex corner (matching the polygon orientation) with no other vertex
         falling inside the candidate ear triangle. *)
      cross a b c *. orient > 0.
      && List.for_all
           (fun j ->
             j = ip || j = ic || j = inx
             ||
             let px, py = pts.(j) in
             not (point_in_triangle px py a b c))
           remaining
    in
    let rec loop remaining acc =
      match remaining with
      | [ a; b; c ] -> (pts.(a), pts.(b), pts.(c)) :: acc
      | _ ->
          let arr = Array.of_list remaining in
          let m = Array.length arr in
          let rec find i =
            if i >= m then
              (* No ear found (degenerate input): clip a vertex anyway so we
                 always make progress and terminate. *)
              (arr.(0), (pts.(arr.(m - 1)), pts.(arr.(0)), pts.(arr.(1))))
            else
              let ip = arr.((i + m - 1) mod m) in
              let ic = arr.(i) in
              let inx = arr.((i + 1) mod m) in
              if is_ear remaining ip ic inx then
                (ic, (pts.(ip), pts.(ic), pts.(inx)))
              else find (i + 1)
          in
          let clipped, tri = find 0 in
          loop (List.filter (fun j -> j <> clipped) remaining) (tri :: acc)
    in
    loop (List.init n (fun i -> i)) []
  end

(* raylib culls triangles whose vertices are not counter-clockwise, so emit each
   triangle with a consistent winding regardless of the input orientation. *)
let draw_triangle_ccw color (ax, ay) (bx, by) (cx, cy) =
  let area = ((bx -. ax) *. (cy -. ay)) -. ((cx -. ax) *. (by -. ay)) in
  let va = v2 ax ay and vb = v2 bx by and vc = v2 cx cy in
  if area > 0. then Raylib.draw_triangle va vc vb color
  else Raylib.draw_triangle va vb vc color

let fill_poly ~io ?color poly =
  let pts = Polygon.points poly in
  if List.length pts >= 3 then begin
    let color = get_color ~io color in
    let pts = List.map (project ~io) pts in
    let tris = triangulate pts in
    with_scissor ~io @@ fun () ->
    List.iter (fun (a, b, c) -> draw_triangle_ccw color a b c) tris
  end

let fill_rect ~io ?color rect =
  let x0, y0 = project ~io (Box.top_left rect) in
  let x1, y1 = project ~io (Box.bottom_right rect) in
  with_scissor ~io @@ fun () ->
  Raylib.draw_rectangle (int_of_float x0) (int_of_float y0)
    (int_of_float (x1 -. x0))
    (int_of_float (y1 -. y0))
    (get_color ~io color)

(* --- SDF circle shaders --- *)

let vs =
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

let fs_fill =
  {glsl|
#version 330
precision mediump float;
uniform vec2 center;
uniform float radius;
uniform vec4 circleColor;
uniform float screenHeight;
out vec4 finalColor;
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float d = length(pos - center) - radius;
    float alpha = 1.0 - smoothstep(-1.0, 1.0, d);
    finalColor = vec4(circleColor.rgb, circleColor.a * alpha);
}
|glsl}

let fs_draw =
  {glsl|
#version 330
precision mediump float;
uniform vec2 center;
uniform float radius;
uniform vec4 circleColor;
uniform float screenHeight;
out vec4 finalColor;
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float d = abs(length(pos - center) - radius) - 0.5;
    float alpha = 1.0 - smoothstep(-1.0, 1.0, d);
    finalColor = vec4(circleColor.rgb, circleColor.a * alpha);
}
|glsl}

type circle_shader = {
  shader : Raylib.Shader.t;
  loc_center : Raylib.ShaderLoc.t;
  loc_radius : Raylib.ShaderLoc.t;
  loc_color : Raylib.ShaderLoc.t;
  loc_screen_height : Raylib.ShaderLoc.t;
}

let load_circle_shader fs =
  let shader = Raylib.load_shader_from_memory vs fs in
  {
    shader;
    loc_center = Raylib.get_shader_location shader "center";
    loc_radius = Raylib.get_shader_location shader "radius";
    loc_color = Raylib.get_shader_location shader "circleColor";
    loc_screen_height = Raylib.get_shader_location shader "screenHeight";
  }

let fill_shader : circle_shader option ref = ref None
let draw_shader : circle_shader option ref = ref None

let get_shader r fs =
  match !r with
  | Some s -> s
  | None ->
      let s = load_circle_shader fs in
      r := Some s;
      s

let buf1 = Ctypes.CArray.make Ctypes.float 1
let buf2 = Ctypes.CArray.make Ctypes.float 2
let buf4 = Ctypes.CArray.make Ctypes.float 4

let set_float shader loc v =
  Ctypes.CArray.set buf1 0 v;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf1 |> to_voidp)
    Raylib.ShaderUniformDataType.Float

let set_vec2 shader loc x y =
  Ctypes.CArray.set buf2 0 x;
  Ctypes.CArray.set buf2 1 y;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf2 |> to_voidp)
    Raylib.ShaderUniformDataType.Vec2

let set_vec4 shader loc x y z w =
  Ctypes.CArray.set buf4 0 x;
  Ctypes.CArray.set buf4 1 y;
  Ctypes.CArray.set buf4 2 z;
  Ctypes.CArray.set buf4 3 w;
  Raylib.set_shader_value shader loc
    Ctypes.(CArray.start buf4 |> to_voidp)
    Raylib.ShaderUniformDataType.Vec4

let circle_uniforms s cx cy radius color =
  set_vec2 s.shader s.loc_center cx cy;
  set_float s.shader s.loc_radius radius;
  let cr = float (Raylib.Color.r color) /. 255.0 in
  let cg = float (Raylib.Color.g color) /. 255.0 in
  let cb = float (Raylib.Color.b color) /. 255.0 in
  let ca = float (Raylib.Color.a color) /. 255.0 in
  set_vec4 s.shader s.loc_color cr cg cb ca;
  set_float s.shader s.loc_screen_height (float (Raylib.get_render_height ()));
  let pad = 2.0 in
  let qx = int_of_float (cx -. radius -. pad) in
  let qy = int_of_float (cy -. radius -. pad) in
  let qs = int_of_float (2.0 *. (radius +. pad)) in
  (qx, qy, qs)

let draw_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let cx, cy = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  let s = get_shader draw_shader fs_draw in
  let qx, qy, qs = circle_uniforms s cx cy radius color in
  with_scissor ~io @@ fun () ->
  Raylib.begin_shader_mode s.shader;
  Raylib.draw_rectangle qx qy qs qs Raylib.Color.white;
  Raylib.end_shader_mode ()

let fill_circle ~io ?color circle =
  let center = Circle.center circle in
  let radius = Circle.radius circle in
  let cx, cy = project ~io center in
  let radius = io.view.Transform.scale *. radius in
  let color = get_color ~io color in
  let s = get_shader fill_shader fs_fill in
  let qx, qy, qs = circle_uniforms s cx cy radius color in
  with_scissor ~io @@ fun () ->
  Raylib.begin_shader_mode s.shader;
  Raylib.draw_rectangle qx qy qs qs Raylib.Color.white;
  Raylib.end_shader_mode ()

(* --- SDF antialiased line shader --- *)

let fs_line =
  {glsl|
#version 330
precision mediump float;
uniform vec2 pa;
uniform vec2 pb;
uniform float halfWidth;
uniform vec4 lineColor;
uniform float screenHeight;
out vec4 finalColor;
float sdSegment(vec2 p, vec2 a, vec2 b) {
    vec2 ap = p - a;
    vec2 ab = b - a;
    float h = clamp(dot(ap, ab) / dot(ab, ab), 0.0, 1.0);
    return length(ap - ab * h);
}
void main() {
    vec2 pos = vec2(gl_FragCoord.x, screenHeight - gl_FragCoord.y);
    float d = sdSegment(pos, pa, pb) - halfWidth;
    float alpha = 1.0 - smoothstep(-0.5, 0.5, d);
    finalColor = vec4(lineColor.rgb, lineColor.a * alpha);
}
|glsl}

type line_shader = {
  l_shader : Raylib.Shader.t;
  l_pa : Raylib.ShaderLoc.t;
  l_pb : Raylib.ShaderLoc.t;
  l_half : Raylib.ShaderLoc.t;
  l_color : Raylib.ShaderLoc.t;
  l_screen_height : Raylib.ShaderLoc.t;
}

let line_shader : line_shader option ref = ref None

let get_line_shader () =
  match !line_shader with
  | Some s -> s
  | None ->
      let shader = Raylib.load_shader_from_memory vs fs_line in
      let s =
        {
          l_shader = shader;
          l_pa = Raylib.get_shader_location shader "pa";
          l_pb = Raylib.get_shader_location shader "pb";
          l_half = Raylib.get_shader_location shader "halfWidth";
          l_color = Raylib.get_shader_location shader "lineColor";
          l_screen_height = Raylib.get_shader_location shader "screenHeight";
        }
      in
      line_shader := Some s;
      s

(* Draw an antialiased segment between two already-projected screen points. *)
let aa_segment color (x0, y0) (x1, y1) =
  let s = get_line_shader () in
  let half_width = 0.5 in
  set_vec2 s.l_shader s.l_pa x0 y0;
  set_vec2 s.l_shader s.l_pb x1 y1;
  set_float s.l_shader s.l_half half_width;
  let cr = float (Raylib.Color.r color) /. 255.0 in
  let cg = float (Raylib.Color.g color) /. 255.0 in
  let cb = float (Raylib.Color.b color) /. 255.0 in
  let ca = float (Raylib.Color.a color) /. 255.0 in
  set_vec4 s.l_shader s.l_color cr cg cb ca;
  set_float s.l_shader s.l_screen_height (float (Raylib.get_render_height ()));
  let pad = half_width +. 2.0 in
  let qx = int_of_float (Float.min x0 x1 -. pad) in
  let qy = int_of_float (Float.min y0 y1 -. pad) in
  let qw = int_of_float (Float.abs (x1 -. x0) +. (2.0 *. pad)) in
  let qh = int_of_float (Float.abs (y1 -. y0) +. (2.0 *. pad)) in
  Raylib.begin_shader_mode s.l_shader;
  Raylib.draw_rectangle qx qy qw qh Raylib.Color.white;
  Raylib.end_shader_mode ()

let draw_line ~io ?color segment =
  let p0, p1 = Segment.to_tuple segment in
  let p0 = project ~io p0 in
  let p1 = project ~io p1 in
  let color = get_color ~io color in
  with_scissor ~io @@ fun () -> aa_segment color p0 p1

let draw_poly ~io ?color poly =
  let pts = Polygon.points poly in
  if List.length pts >= 2 then begin
    let color = get_color ~io color in
    let pts = List.map (project ~io) pts in
    let segments =
      match pts with
      | [] -> []
      | first :: _ ->
          let rec pairs = function
            | a :: (b :: _ as rest) -> (a, b) :: pairs rest
            | [ last ] -> [ (last, first) ]
            | [] -> []
          in
          pairs pts
    in
    with_scissor ~io @@ fun () ->
    List.iter (fun (a, b) -> aa_segment color a b) segments
  end

let draw_rect ~io ?color rect =
  draw_poly ~io ?color
    begin
      Polygon.v
        [
          Box.top_left rect;
          Box.top_right rect;
          Box.bottom_right rect;
          Box.bottom_left rect;
        ]
    end
