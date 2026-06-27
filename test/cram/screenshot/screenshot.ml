open Webdriver_cohttp_lwt_unix
open Infix

let test =
  let* () = goto (Printf.sprintf "file://%s/%s" (Sys.getcwd ()) Sys.argv.(1)) in
  Unix.sleepf 0.5;
  let* canvas = find_first `tag_name "canvas" in
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
