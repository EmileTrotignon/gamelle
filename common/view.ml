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
  window_size : (int * int) ref;
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
