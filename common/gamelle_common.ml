module Transform = Transform
module View = View
module Events_backend = Events_backend
module Event_query = Event_query
module Font = Font
module Font_metrics = Font_metrics
module Delayed = Delayed
module Geometry = Geometry

let max_sounds = 256

open Geometry

type 'a abstract_io = 'a View.abstract_io = {
  view : Transform.t;
  event : Events_backend.t ref;
  clip : box option;
  clip_events : bool;
  z_index : int;
  color : Color.t;
  window_size : (int * int) ref;
  clean : (unit -> unit) list ref;
  draws : (int * (unit -> unit)) list ref;
  backend : 'a;
}

let make_io backend =
  {
    view = Transform.default;
    event = ref Events_backend.default;
    clip = None;
    clip_events = false;
    z_index = 0;
    color = Color.white;
    window_size = ref (0, 0);
    clean = ref [];
    draws = ref [];
    backend;
  }

let make_io ?previous backend =
  let io = make_io backend in
  match previous with
  | None -> io
  | Some previous -> { io with window_size = previous.window_size }

let io_reset_mutable_fields io =
  io.event := Events_backend.default;
  io.window_size := (0, 0);
  io.clean := [];
  io.draws := []

let clean_io ~io fn = io.clean := fn :: !(io.clean)
let get_color ~io = function None -> io.color | Some c -> c
let clock ~io = Events_backend.clock !(io.event)
let dt ~io = Events_backend.dt !(io.event)
let z ~io f = io.draws := (io.z_index, fun () -> f ~io) :: !(io.draws)

let finalize_frame ~io =
  !(io.draws)
  |> List.stable_sort (fun (z, _) (z', _) -> -Int.compare z z')
  |> List.rev
  |> List.iter (fun (_z, f) -> f ())

let ui_replay_height = ref 0.0
