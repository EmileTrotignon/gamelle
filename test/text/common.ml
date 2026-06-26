open Gamelle

(* Shared infrastructure for the screenshot scenarios. Each scenario lives in its
   own module and renders text wrapped in a red box of its measured [Text.size] /
   [Text.size_multiline], so the PNG shows the rendering against the predicted
   extent. Instead of reading internal glyph positions, the cross-backend test
   asserts that both backends predict the same sizes; the predictions are
   recorded here and flushed by the entry points (the native exe to a file, the
   browser to a window global). *)

let width = 640.
let height = 640.

(* Roboto: a proportional font with a GPOS table (kerning) and hhea/head/OS-2
   metrics that disagree, used to test whether the raylib/browser size matching
   generalises beyond the monospaced default font. *)
let roboto = Font.load [%blob "Roboto-Regular.ttf"]

(* Recorded size predictions, one text per line. *)
let log = Buffer.create 256

let record label sz =
  Buffer.add_string log
    (Printf.sprintf "%s -> w=%.3f h=%.3f\n" label (Size.width sz)
       (Size.height sz))

let dump () = Buffer.contents log

(* Native flush: write the recorded predictions to [GAMELLE_SIZE_LOG] if set.
   Pure stdlib, so it is a harmless no-op in the browser (the variable is unset
   there). *)
let flush_file () =
  match Sys.getenv_opt "GAMELLE_SIZE_LOG" with
  | Some p -> (
      try
        let oc = open_out p in
        output_string oc (dump ());
        close_out oc
      with _ -> ())
  | None -> ()

(* Start a scenario: reset the log, fix a [w]x[h] drawing box and clear it to
   white. Returns the scoped [io]. *)
let setup ~io w h =
  Buffer.clear log;
  let io =
    View.drawing_box ~set_window_size:true (Box.v Point.zero (Size.v w h)) io
  in
  Box.fill ~io ~color:Color.white (Window.box ~io);
  io

(* Draw [text] at [at], box its measured size, record it; return its height. *)
let draw_sample ~io ?font ~at ~size text =
  let s = Text.size ~io ?font ~size text in
  record (Printf.sprintf "size=%d %S" size text) s;
  Box.draw ~io ~color:Color.red (Box.v at s);
  Text.draw ~io ?font ~color:Color.black ~size ~at text;
  Size.height s
