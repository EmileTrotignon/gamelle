(* SVG is unsupported on the deprecated SDL backend (raylib and jsoo are the
   maintained pair). *)

type t = unit

let load ~w:_ ~h:_ _ =
  failwith "gamelle: SVG is not supported by the SDL backend"
