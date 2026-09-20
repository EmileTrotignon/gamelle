open Common
open Gamelle_common
module Transform = Gamelle_common.Transform

let tau = 8.0 *. atan 1.0

type t = { rt : Raylib.RenderTexture.t; w : int; h : int }

(* [load_render_texture] attaches only a depth renderbuffer, so the FBO has no
   stencil bits and [with_scissor]'s stencil clip path degrades to its
   axis-aligned scissor (a box clip survives because its bounding box equals the
   box; a rotated or concave clip becomes its bounding box). Replace the depth
   attachment with a packed depth+stencil renderbuffer so clipping inside a
   texture works exactly as it does on screen. The original depth renderbuffer is
   left detached (freed in [free], below). *)
let attach_stencil rt w h =
  Raylib.Rlgl.enable_framebuffer (Raylib.RenderTexture.id rt);
  Gl.framebuffer_renderbuffer Gl.framebuffer Gl.depth_attachment Gl.renderbuffer
    0;
  let p = Ctypes.allocate Ctypes.int 0 in
  Gl.gen_renderbuffers 1 p;
  let rb = Ctypes.( !@ ) p in
  Gl.bind_renderbuffer Gl.renderbuffer rb;
  Gl.renderbuffer_storage Gl.renderbuffer Gl.depth24_stencil8 w h;
  Gl.framebuffer_renderbuffer Gl.framebuffer Gl.depth_stencil_attachment
    Gl.renderbuffer rb;
  Gl.bind_renderbuffer Gl.renderbuffer 0;
  Raylib.Rlgl.disable_framebuffer ()

(* GPU buffers of textures that have been garbage-collected, waiting to have their
   OpenGL resources deleted. The finalizer only enqueues (it may run on any
   domain); [free_pending], called once per frame from the run loop, does the
   actual deletes on the main domain where the GL context is current. *)
let to_free : Raylib.RenderTexture.t list ref = ref []
let to_free_mutex = Mutex.create ()

let delete_renderbuffer id =
  let p = Ctypes.allocate Ctypes.int id in
  Gl.delete_renderbuffers 1 p

(* Delete the GL resources of every collected render texture. [unload_render_texture]
   frees the FBO, its colour texture and — via its depth-attachment query — the
   packed depth+stencil renderbuffer we attached; the original depth renderbuffer,
   which [attach_stencil] detached, is freed separately here. *)
let free_pending () =
  let rts =
    Mutex.protect to_free_mutex (fun () ->
        let l = !to_free in
        to_free := [];
        l)
  in
  List.iter
    (fun rt ->
      let depth_id =
        Unsigned.UInt.to_int
          (Raylib.TextureId.to_uint
             (Raylib.Texture.id (Raylib.RenderTexture.depth rt)))
      in
      Raylib.unload_render_texture rt;
      delete_renderbuffer depth_id)
    rts

(* Texture renders have no outer [io] to borrow font state from, so they use the
   default font; a render function can still switch fonts with [View.font]. *)
let default_backend = { font = Font_.default; font_size = Font_.default_size }

(* Reset the buffer to fully transparent. *)
let wipe t =
  Raylib.begin_texture_mode t.rt;
  Raylib.clear_background Raylib.Color.blank;
  Raylib.end_texture_mode ()

(* Allocate a fresh [w]x[h] render texture, cleared to transparent. Its GPU
   resources are reclaimed automatically: when the returned [t] is
   garbage-collected, a finalizer queues them for deletion on the next frame. *)
let create ~w ~h =
  let rt = Raylib.load_render_texture w h in
  attach_stencil rt w h;
  let t = { rt; w; h } in
  wipe t;
  Gc.finalise
    (fun t ->
      Mutex.protect to_free_mutex (fun () -> to_free := t.rt :: !to_free))
    t;
  t

(* Render [draw_fn] onto [t], over whatever it already holds (no clear, so
   successive calls accumulate).

   [draw_fn] gets its own [io] — a fresh drawing context with a default view, an
   empty clip and its own deferred-draw list. Its queued draws are flushed here
   with [finalize_frame], so their internal z-ordering is resolved as they are
   composited onto the texture.

   The render texture has no MSAA, so straight-edged fills come out aliased; the
   SDF-based primitives (circles, lines, arcs, rounded rects) still antialias
   themselves, once [render_height_override] points their [screenHeight] at the
   texture's height rather than the window's. *)
let draw_on t draw_fn =
  let tio =
    {
      (Gamelle_common.make_io default_backend) with
      render_target =
        Gamelle_common.View.Texture { width = t.w; height = t.h };
    }
  in
  let prev = !render_height_override in
  render_height_override := Some t.h;
  Raylib.begin_texture_mode t.rt;
  draw_fn ~io:tio;
  Gamelle_common.finalize_frame ~io:tio;
  Raylib.end_texture_mode ();
  render_height_override := prev

(* Draw the texture as the same transformed quad as a bitmap. A render texture is
   stored bottom-up (its FBO origin is bottom-left), so the source rectangle uses
   a negative height to flip it back into screen orientation. *)
let draw ~io t p =
  let scale = io.view.Transform.scale in
  let x, y = project ~io p in
  let tex = Raylib.RenderTexture.texture t.rt in
  let src = Raylib.Rectangle.create 0. 0. (float t.w) (-.float t.h) in
  let dst =
    Raylib.Rectangle.create x y (float t.w *. scale) (float t.h *. scale)
  in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  with_scissor ~io @@ fun () ->
  Raylib.draw_texture_pro tex src dst (v2 0. 0.) angle Raylib.Color.white
