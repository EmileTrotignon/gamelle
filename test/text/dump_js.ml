(* Browser (jsoo) entry point — the single js file, linked only into the js
   executable. The scenario is derived from the page filename (e.g.
   ".../roboto_glyph.html" -> "roboto_glyph"); after rendering, the recorded
   Text.size predictions are published on [window.gamelle_sizes] for the dump
   tool to read back. *)
open Scenes

let scenario =
  try
    let path = Jv.to_string (Jv.get (Jv.get Jv.global "location") "pathname") in
    Filename.remove_extension (Filename.basename path)
  with _ -> "lines"

let render ~io () =
  match scenario with
  | "glyph" -> Glyph.render ~io ()
  | "roboto" -> Roboto.render ~io ()
  | "roboto_glyph" -> Roboto_glyph.render ~io ()
  | "lines" | _ -> Lines.render ~io ()

let () =
  Gamelle.run () (fun ~io () ->
      render ~io ();
      Jv.set Jv.global "gamelle_sizes" (Jv.of_string (Common.dump ())))
