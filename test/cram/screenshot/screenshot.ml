open Webdriver_cohttp_lwt_unix
open Infix

let num = function
  | `Int i -> float_of_int i
  | `Float f -> f
  | `Intlit s -> float_of_string s
  | _ -> 0.

let test =
  let* () = goto (Printf.sprintf "file://%s/%s" (Sys.getcwd ()) Sys.argv.(1)) in
  Unix.sleepf 0.5;
  let* canvas = find_first `tag_name "canvas" in
  (* A WebDriver element screenshot only captures what fits inside the viewport:
     for a canvas taller than the default ~680px headless window, geckodriver
     clips it (on CI it returned just an 680px-tall band; a real display happened
     to back a large enough surface locally, which is why this passed there but
     not in CI). Grow the window to contain the whole canvas first, so the full
     scene is captured on any machine. *)
  let* size =
    execute
      "var c = document.querySelector('canvas').getBoundingClientRect(); \
       return [c.width, c.height];"
  in
  let w, h =
    match size with `List [ a; b ] -> (num a, num b) | _ -> (0., 0.)
  in
  let* _ =
    Window.set_rect
      {
        Window.x = 0;
        y = 0;
        width = int_of_float w + 200;
        height = int_of_float h + 300;
      }
  in
  Unix.sleepf 0.3;
  let* img = screenshot ~elt:canvas () in
  return img

(* Port of the geckodriver to drive; passed so parallel runs can each use their
   own (defaults to 4444). *)
let host =
  let port = if Array.length Sys.argv > 2 then Sys.argv.(2) else "4444" in
  Printf.sprintf "http://127.0.0.1:%s" port

let () =
  try
    Lwt_main.run
      (let ( let* ) = Lwt.bind in
       let* str = run ~host Capabilities.firefox_headless test in
       Lwt.return @@ print_string str)
  with Webdriver e ->
    Printf.fprintf stderr "[FAIL] Webdriver error: %s\n%!" (Error.to_string e);
    Printexc.print_backtrace stderr;
    Printf.fprintf stderr "\n%!"
