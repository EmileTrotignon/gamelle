(* The backend-free core of gamelle: geometry and rigid-body physics, with no
   rendering and no C dependency. Link this (instead of the virtual [gamelle]
   library) in headless programs such as game servers, so they don't have to
   pull in a graphics backend. [Gamelle] re-exports all of it. *)

include Gamelle_common.Geometry
module Physics = Physics
