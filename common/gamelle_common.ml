module Transform = Transform
module View = View
module Events_backend = Events_backend
module Input_snapshot = Input_snapshot
module Font = Font
module Font_metrics = Font_metrics
module Delayed = Delayed
module Geometry = Geometry
module Netcode = Netcode

let max_sounds = 256

open Geometry

type 'a abstract_io = 'a View.abstract_io = {
  view : Transform.t;
  event : Events_backend.t ref;
  clip : Polygon.t list;
  clip_events : bool;
  z_index : int;
  color : Color.t;
  render_target : View.render_target;
  clean : (unit -> unit) list ref;
  draws : (int * (unit -> unit)) list ref;
  backend : 'a;
}

let make_io backend =
  {
    view = Transform.default;
    event = ref Events_backend.default;
    clip = [];
    clip_events = false;
    z_index = 0;
    color = Color.white;
    render_target = View.Onscreen { requested_size = None };
    clean = ref [];
    draws = ref [];
    backend;
  }

let io_reset_mutable_fields io =
  io.event := Events_backend.default;
  (match io.render_target with
  | View.Onscreen o -> o.requested_size <- None
  | View.Texture _ -> ());
  io.clean := [];
  io.draws := []

let clean_io ~io fn = io.clean := fn :: !(io.clean)
let get_color ~io = function None -> io.color | Some c -> c
let clock ~io = Events_backend.clock !(io.event)
let ticks ~io = Events_backend.ticks !(io.event)
let dt ~io = Events_backend.dt !(io.event)
let target_dt = Events_backend.target_dt
let z ~io f = io.draws := (io.z_index, fun () -> f ~io) :: !(io.draws)

let finalize_frame ~io =
  !(io.draws)
  |> List.stable_sort (fun (z, _) (z', _) -> -Int.compare z z')
  |> List.rev
  |> List.iter (fun (_z, f) -> f ())

let ui_replay_height = ref 0.0
