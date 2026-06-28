let () = Random.init 0

module Ui = Ui
module Ease = Ease
module Anim = Anim
module Physics = Physics
module Routine = Routine
module View = View
module Transform = Gamelle_common.Transform
module Input = Event
module Bitmap = Bitmap_
include Draw_geometry

let dt = Gamelle_backend.dt
let clock = Gamelle_backend.clock

module Sound = Gamelle_backend.Sound
module Window = Window_
module Clipboard = Gamelle_backend.Clipboard

type io = Gamelle_backend.io

(** raw run function, without the handler for {!next_frame}. *)
let run_no_handler init f =
  Gamelle_backend.run init (fun ~io ->
      Box.fill ~io ~color:Color.black (Window.box ~io);
      let r = f ~io in
      Gamelle_common.finalize_frame ~io;
      r)

(* code from routine.ml, modified to have only type unit and for the next_frame
function to be top-level (which is possible because of the monomorphic type)*)

type 'i eff += Wait_for_next_frame : unit -> unit eff

let next_frame ~(io : io) =
  let _ = io in
  Effect.perform (Wait_for_next_frame ())

type ('i, 'o, 'a) state =
  | Finished of 'a
  | Running of 'o * ('i, 'o, 'a) continuation
  | Start

and ('i, 'o, 'a) continuation =
  ('i, ('i, 'o, 'a) state) Effect.Deep.continuation

let run_no_loop (f : io:io -> unit) : unit =
  let routine : (unit, unit, unit) state = Start in
  run_no_handler (routine : (unit, unit, unit) state) begin fun ~io state ->
    match state with
    | Start ->
        begin try Finished (f ~io)
        with effect Wait_for_next_frame (), k ->
          Running ((), (k : (unit, _) Effect.Deep.continuation))
        end
    | Running (_o, k) -> Effect.Deep.continue k ()
    | Finished v -> Finished v
    end

let run init f =
  let rec loop ~io state =
    let state = f ~io state in
    next_frame ~io;
    loop ~io state
  in
  run_no_loop (loop init)

module Font = Gamelle_backend.Font

let draw_string ~io ?color ?font ?size ~at txt =
  Text.draw ~io ?color ?font ?size ~at txt
