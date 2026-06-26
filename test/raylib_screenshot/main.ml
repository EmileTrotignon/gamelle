(* Native (raylib) entry point. The scenario name is the first command-line
   argument (default "lines"); after rendering, the recorded Text.size
   predictions are flushed to GAMELLE_SIZE_LOG if set. *)
open Scenes

let scenario = if Array.length Sys.argv > 1 then Sys.argv.(1) else "lines"

let render ~io () =
  match scenario with
  | "glyph" -> Glyph.render ~io ()
  | "roboto" -> Roboto.render ~io ()
  | "roboto_glyph" -> Roboto_glyph.render ~io ()
  | "lines" | _ -> Lines.render ~io ()

let () =
  Gamelle.run () (fun ~io () ->
      render ~io ();
      Common.flush_file ())
