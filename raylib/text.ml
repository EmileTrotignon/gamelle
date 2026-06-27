type t = string

let length s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun len _ -> len + 1) 0 s

let sub s start n =
  let inner s start n acc =
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      begin fun (pos, collected, s) c ->
        if pos >= start && collected < n then (pos + 1, collected + 1, s ^ c)
        else (pos + 1, collected, s)
      end
      acc s
  in
  let _, _, r = inner s start n (0, 0, "") in
  r

let get txt i = sub txt i 1

let chars s =
  s
  |> Uuseg_string.fold_utf_8 `Grapheme_cluster (fun li c -> c :: li) []
  |> List.rev

let ( ^ ) = ( ^ )

let slice ?(start = 0) ?stop str =
  let len = length str in
  let stop = match stop with Some stop -> stop | None -> len - start in
  let new_len = if stop < 0 then len - stop else stop - start in
  sub str start new_len

let to_string = Fun.id
let of_string = Fun.id

let draw_glyph ~io ?color ?font ?size ~at:p text =
  if text <> "" then Font_.draw_glyph ~io ?color ?font ?size ~at:p text

let size ~io ?font ?size text = Font_.text_size ~io ?font ?size text
