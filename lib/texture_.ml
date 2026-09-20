module B = Gamelle_backend.Texture
module Size = Gamelle_common.Geometry.Size

(* A texture: a fixed-size offscreen buffer that can be drawn onto with
   {!draw_on_texture} and blitted to the screen with {!draw_on_screen}. The
   backend buffer [buf] is allocated lazily on first use, so a texture can be
   created (with {!v}) at top level, before the graphics context exists. *)
type t = { w : int; h : int; mutable buf : B.t option }

let create size =
  {
    w = int_of_float (Size.width size);
    h = int_of_float (Size.height size);
    buf = None;
  }

let size t = Size.v (float t.w) (float t.h)
let width t = t.w
let height t = t.h

let ensure t =
  match t.buf with
  | Some b -> b
  | None ->
      let b = B.create ~w:t.w ~h:t.h in
      t.buf <- Some b;
      b

(* Render onto the texture, over whatever it already holds. Allocates the backend
   buffer on first call, so it must run within the game loop, not before [run]. *)
let draw_on_texture t f = B.draw_on (ensure t) f

(* Reset the texture to empty (fully transparent). A texture never drawn onto is
   already empty, so there is nothing to wipe. *)
let wipe t = match t.buf with None -> () | Some b -> B.wipe b

(* Blit the texture at [at] as a single deferred draw, so it occupies the [io]'s
   current z-index (its own internal z-ordering having been collapsed as it was
   drawn onto). A texture never drawn onto is empty (transparent), so nothing is
   emitted. *)
let draw_on_screen ~io ~at t =
  match t.buf with
  | None -> ()
  | Some b -> Gamelle_common.z ~io (fun ~io -> B.draw ~io b at)
