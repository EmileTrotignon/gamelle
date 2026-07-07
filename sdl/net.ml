(* TODO: networking is not implemented on the SDL backend. *)
type t = unit
type status = Connecting | Connected | Closed | Error of string

let todo () = failwith "Net is not implemented on the SDL backend (TODO)"
let connect _ = todo ()
let send _ _ = todo ()
let poll _ = todo ()
let status _ = todo ()
let is_connected _ = todo ()
let close _ = todo ()
