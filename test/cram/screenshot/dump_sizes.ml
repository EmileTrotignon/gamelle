(* Loads the page like screenshot.exe, but instead of a PNG it prints the text
   size predictions the scene recorded on [window.gamelle_sizes] (see the
   screenshot test scene). *)
open Webdriver_cohttp_lwt_unix
open Infix

let test =
  let* () = goto (Printf.sprintf "file://%s/%s" (Sys.getcwd ()) Sys.argv.(1)) in
  Unix.sleepf 0.7;
  let* j = execute "return (window.gamelle_sizes || '');" in
  return (match j with `String s -> s | _ -> "")

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
    exit 1
