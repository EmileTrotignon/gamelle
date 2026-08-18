open Utils

type loader = Raw of string | Parts of string * string * string list

(* usvg (from the resvg project) inlines <use>/<defs>/<symbol> references and
   flattens every shape to a plain <path>. We run SVG assets through it at pack
   time so the raylib backend's nanosvg rasteriser — which does not resolve <use>
   — draws the whole image rather than only its directly-authored elements (the
   symptom being an SVG exported from Flash/SWF showing just its background).
   Tools like FFDec emit every layer as a <use>, so without this such assets are
   near-empty in the raylib backend. *)
let usvg_available = lazy (Sys.command "usvg --version >/dev/null 2>&1" = 0)

(* Warn at most once, rather than per SVG, when usvg is missing. *)
let warned_no_usvg = ref false

let run_usvg sysname =
  let cmd = Filename.quote_command "usvg" [ sysname; "-c" ] in
  let ic = Unix.open_process_in cmd in
  let out = In_channel.input_all ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Some out
  | _ ->
      Format.eprintf
        "gamelle: warning: usvg failed on %s; embedding it unchanged (it may \
         render only partially in the raylib backend).@."
        sysname;
      None

(* The SVG source to embed for [sysname]: inlined by usvg when available, else
   the file unchanged (with a one-time warning explaining the consequence). *)
let svg_payload sysname =
  if Lazy.force usvg_available then
    Option.value (run_usvg sysname) ~default:(file_contents sysname)
  else begin
    if not !warned_no_usvg then begin
      warned_no_usvg := true;
      Format.eprintf
        "gamelle: warning: command 'usvg' was not found, so SVG assets are \
         embedded as-is. <use>/<defs> or text node will not be shown in the \
         raylib backend: the nanosvg rasteriser does not support these \
         SVG features. "
    end;
    file_contents sysname
  end

(* Returns the loader expression and the payload string to embed after it. *)
let extension_loader ~sysname ~ext =
  match ext with
  | ".ttf" -> Some (Raw "Gamelle.Font.load", file_contents sysname)
  | ".png" | ".jpeg" | ".jpg" ->
      let chunk_reader = ImageUtil_unix.chunk_reader_of_path sysname in
      let w, h =
        ImageLib.size
          ~extension:(String.sub ext 1 (String.length ext - 1))
          chunk_reader
      in
      let raw = Printf.sprintf "Gamelle.Bitmap.load ~w:%i ~h:%i" w h in
      let parts = sysname ^ ".parts" in
      if Sys.file_exists parts then
        let parts =
          file_contents parts |> String.split_on_char '\n'
          |> List.filter (( <> ) "")
          |> List.rev
        in
        let parts =
          List.map
            (fun line ->
              match String.split_on_char ' ' line with
              | [ x; y; w; h ] ->
                  Printf.sprintf "~x:(%s) ~y:(%s) ~w:(%s) ~h:(%s)" x y w h
              | _ -> line)
            parts
        in
        Some (Parts (raw, "Gamelle.Bitmap.sub", parts), file_contents sysname)
      else Some (Raw raw, file_contents sysname)
  | ".svg" ->
      let svg = svg_payload sysname in
      let w, h =
        match Nanosvg.parse svg with
        | Some img ->
            ( max 1 (int_of_float (Float.round (Nanosvg.Image_data.width img))),
              max 1 (int_of_float (Float.round (Nanosvg.Image_data.height img)))
            )
        | None -> (1, 1)
      in
      Some (Raw (Printf.sprintf "Gamelle.Svg.load ~w:%i ~h:%i" w h), svg)
  | ".mp3" | ".wav" | ".ogg" | ".flac" ->
      Some (Raw "Gamelle.Sound.load", file_contents sysname)
  | _ -> Some (Raw "Fun.id", file_contents sysname)

let rec traverse sysname =
  let name =
    normalize_name @@ Filename.remove_extension @@ Filename.basename sysname
  in
  if is_directory sysname then (
    let name = String.capitalize_ascii name in
    Format.printf "module %s = struct@." name;
    let lst = Sys.readdir sysname in
    Array.iter (fun child -> traverse (Filename.concat sysname child)) lst;
    Format.printf "end@.")
  else if is_regular_file sysname then
    let ext = Filename.extension sysname in
    match extension_loader ~sysname ~ext with
    | Some (Raw loader, payload) ->
        Format.printf "  let %s = %s %S@." name loader payload
    | Some (Parts (loader, extract, parts), payload) ->
        Format.printf "  let %s =\n" name;
        Format.printf "    let raw = %s %S in\n" loader payload;
        Format.printf "    [|\n";
        List.iter (Format.printf "      %s raw %s;\n" extract) parts;
        Format.printf "    |]@."
    | None -> ()

let run () =
  let cwd = Sys.getcwd () in
  Array.iter traverse (Sys.readdir cwd)
