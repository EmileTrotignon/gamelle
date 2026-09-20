(* What an [io] renders into, fixed when the [io] is created. The two cases are
   kept apart so an offscreen buffer can never carry a window-resize request, and
   so querying an [io]'s size is a direct lookup rather than a guess. *)
type render_target =
  | Onscreen of { mutable requested_size : (int * int) option }
      (* The game window / browser canvas. [requested_size] is a resize the game
         asked for this frame via [Window.set_size]; the backend applies and
         clears it each frame. [None] means no resize was requested. *)
  | Texture of { width : int; height : int }
      (* A fixed-size offscreen buffer: a [Texture.t] being rendered. It cannot
         be resized, so it has no [requested_size]. *)

type 'a abstract_io = {
  view : Transform.t;
  event : Events_backend.t ref;
  (* The active clip regions, each frozen into screen space at the moment it
     was applied. A box projected through a rotated view is a parallelogram, so
     a region is a polygon rather than a box, and regions do not move when the
     view is later translated, scaled or rotated. Successive clips accumulate
     here (most recent first): a pixel is drawn only if it lies inside every
     region, so each new clip can only shrink the visible zone, never enlarge
     it. An empty list means no clipping. *)
  clip : Geometry.Polygon.t list;
  clip_events : bool;
  z_index : int;
  color : Color_.t;
  render_target : render_target;
  clean : (unit -> unit) list ref;
  draws : (int * (unit -> unit)) list ref;
  backend : 'a;
}

let translation io = io.view.translate

(* The clip region is frozen in screen space when applied, so later view
   changes leave it untouched — they only affect what is drawn, not where it
   is clipped. *)
let translate dxy io = { io with view = Transform.translate dxy io.view }
let scale factor io = { io with view = Transform.scale factor io.view }
let rotate angle io = { io with view = Transform.rotate angle io.view }

(* A clip only shrinks the visible zone: the new region is intersected with
   whatever is already active by prepending it to the list, never replacing it. *)
let clip box io =
  {
    io with
    clip =
      Transform.project_polygon io.view (Geometry.Polygon.of_box box) :: io.clip;
  }

let clip_polygon poly io =
  { io with clip = Transform.project_polygon io.view poly :: io.clip }

let clip_events b io = { io with clip_events = b }
let z_index z io = { io with z_index = z }
let color c io = { io with color = c }
