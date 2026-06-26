open Common
open Gamelle_common
open Geometry
module Delayed = Gamelle_common.Delayed

let get_color ~io c =
  let c = Gamelle_common.get_color ~io c in
  to_raylib_color c

type t = font

let set_font font io = { io with font }
let set_font_size font_size io = { io with font_size }

let load binstring =
  Delayed.make @@ fun ~io:_ ->
  {
    data = binstring;
    sizes = Hashtbl.create 16;
    metrics = Font_metrics.of_ttf binstring;
  }

let default : t = load Gamelle_common.Font.default
let default_size = Gamelle_common.Font.default_size

(* Initial codepoint range loaded eagerly: ASCII + Latin extended + Greek + Cyrillic. *)
let initial_codepoints = Array.init (1103 - 32 + 1) (fun i -> 32 + i)

let load_font_with_codepoints data size codepoints =
  let n = Array.length codepoints in
  let arr = Ctypes.CArray.make Ctypes.int n in
  Array.iteri (fun i cp -> Ctypes.CArray.set arr i cp) codepoints;
  let f =
    Raylib.load_font_from_memory ".ttf" data (String.length data) size
      (Ctypes.CArray.start arr) n
  in
  assert (Raylib.is_font_valid f);
  Raylib.set_texture_filter (Raylib.Font.texture f) Raylib.TextureFilter.Point;
  f

(* Reload the font if any codepoint in [text] is not yet in the atlas. *)
let ensure_codepoints font_s sf size text =
  let missing = ref [] in
  Font_metrics.iter_codepoints text begin fun cp ->
      if not (Hashtbl.mem sf.codepoint_set cp) then missing := cp :: !missing
    end;
  if !missing <> [] then begin
    List.iter (fun cp -> Hashtbl.replace sf.codepoint_set cp ()) !missing;
    let all_cps =
      Array.of_list
        (Hashtbl.fold (fun cp () acc -> cp :: acc) sf.codepoint_set [])
    in
    Raylib.unload_font sf.raylib_font;
    sf.raylib_font <- load_font_with_codepoints font_s.data size all_cps
  end

let get_sized_font ~io font size =
  let font_s = Delayed.force ~io font in
  match Hashtbl.find_opt font_s.sizes size with
  | Some sf -> (font_s, sf)
  | None ->
      let cp_set = Hashtbl.create 2048 in
      Array.iter (fun cp -> Hashtbl.replace cp_set cp ()) initial_codepoints;
      let raylib_font =
        load_font_with_codepoints font_s.data size initial_codepoints
      in
      let sf = { raylib_font; codepoint_set = cp_set } in
      clean_io ~io (fun () ->
          Hashtbl.remove font_s.sizes size;
          Raylib.unload_font sf.raylib_font);
      Hashtbl.replace font_s.sizes size sf;
      (font_s, sf)

let get ~io font_opt size_opt text =
  let font, req_size = get_font ~io font_opt size_opt in
  let font_s = Delayed.force ~io font in
  (* raylib/stb size fonts by pixel height (ascent - descent). Rasterise the
     atlas at the whole-pixel height [Font_metrics.pixel_height] and draw glyphs
     1:1 (stb has no hinting, so it is only crisp on the pixel grid); the browser
     snaps to the same height, so the two backends render at exactly the same
     size. https://github.com/raysan5/raylib/issues/3766 *)
  let atlas_px = Font_metrics.pixel_height font_s.metrics req_size in
  let font_s, sf = get_sized_font ~io font atlas_px in
  ensure_codepoints font_s sf atlas_px text;
  (sf.raylib_font, req_size, font_s)

let text_size ~io ?font ?size text =
  let _raylib_font, req_size, font_s = get ~io font size text in
  let w, h = Font_metrics.text_size font_s.metrics req_size text in
  Size.v w h

let tau = 8.0 *. atan 1.0

(* Draw a single glyph (the first codepoint of [text]) with its layout origin at
   [p]; inter-glyph advancing is done in the shared Draw_geometry layout. The
   glyph is drawn at the atlas' integer pixel height (a 1:1 blit; stb has no
   hinting, so it is only crisp at whole sizes/positions). *)
let draw_glyph ~io ?color ?font ?size ~at:p text =
  let raylib_font, req_size, font_s = get ~io font size text in
  let m = font_s.metrics in
  let glyph_size = float_of_int (Font_metrics.pixel_height m req_size) in
  let color = get_color ~io color in
  let x, y = project ~io p in
  let angle = io.view.Transform.rotate *. 360.0 /. tau in
  let cp =
    Uchar.to_int (Uchar.utf_decode_uchar (String.get_utf_8_uchar text 0))
  in
  with_scissor ~io begin fun () ->
      if angle = 0.0 then begin
        (* Offset the glyph top so the baseline lands on the ceiled ascent. *)
        let top =
          Float.round
            (y
            +. Font_metrics.line_ascent m req_size
            -. Font_metrics.raw_ascent m req_size)
        in
        Raylib.draw_text_codepoint raylib_font cp
          (Raylib.Vector2.create (Float.round x) top)
          glyph_size color
      end
      else begin
        (* Rotated text can't be pixel-snapped; place it via the rlgl matrix. *)
        Raylib.Rlgl.push_matrix ();
        Raylib.Rlgl.translatef x y 0.0;
        Raylib.Rlgl.rotatef angle 0.0 0.0 1.0;
        Raylib.draw_text_codepoint raylib_font cp
          (Raylib.Vector2.create 0.0 0.0)
          glyph_size color;
        Raylib.Rlgl.pop_matrix ()
      end
    end
