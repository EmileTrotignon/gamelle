open Brr
open Jsoo
open Gamelle_common
open Geometry

type t = font

let set_font font io = { io with font }
let set_font_size font_size io = { io with font_size }

let get_font ~io = function
  | Some (lazy font) -> font
  | None -> Lazy.force io.font

let get_font_size ~io = function Some size -> size | None -> io.font_size

let get_font ~io font size =
  let io = io.backend in
  (get_font ~io font, get_font_size ~io size)

let uid = ref 0

let gen () =
  let u = !uid in
  uid := u + 1;
  u

(* Sizing/advance metrics (shared with the raylib backend, see Font_metrics),
   keyed by the generated FontFace name. The browser lays text out exactly like
   raylib — same hhea vertical metrics, same per-glyph hmtx advances, same integer
   snapping — rather than using the canvas' native layout (whose fontBoundingBox
   split and kerning diverge), so glyph positions match across backends. *)
let metrics_table : (string, Font_metrics.t) Hashtbl.t = Hashtbl.create 8

let metrics_of name =
  match Hashtbl.find_opt metrics_table name with
  | Some m -> m
  | None -> failwith ("gamelle: font metrics not loaded: " ^ name)

let load binstring =
  lazy begin
    let name = "GamelleFont" ^ string_of_int (gen ()) in
    Hashtbl.replace metrics_table name (Font_metrics.of_ttf binstring);
    let arr = Bitmap.tarray_of_string binstring in
    let fontface_class = Jv.get Jv.global "FontFace" in
    let ttf =
      Jv.new' fontface_class
        [| Jv.of_string name; Tarray.(to_jv (of_bigarray1 arr)) |]
    in
    let loading = Jv.call ttf "load" [||] in
    let _ =
      Jv.call loading "then"
        [|
          Jv.callback ~arity:1 (fun fontface ->
              let fonts = Jv.get (Document.to_jv G.document) "fonts" in
              let _res = Jv.call fonts "add" [| fontface |] in
              ());
          Jv.callback ~arity:1 (fun e -> Console.(log [ "font error:"; e ]));
        |]
    in
    name
  end

let default = load Gamelle_common.Font.default
let default_size = Gamelle_common.Font.default_size

(* Set the css font so one em is [Font_metrics.em_px] pixels — the same per-em
   pixel size (snapped to a whole pixel height) the raylib atlas uses. *)
let set_snapped_font ~io m font_name size =
  let css = Font_metrics.em_px m size in
  C.set_font io.backend.ctx
    (Jstr.of_string (Printf.sprintf "%gpx %s" css font_name))

(* Draw a single glyph; inter-glyph advancing is done in the shared
   Draw_geometry layout. Drawing one glyph at a time means the canvas applies no
   kerning, so positions match raylib byte-for-byte. *)
let draw_at ~io ?color ?font ?size ~at text =
  let font_name, size = get_font ~io font size in
  let m = metrics_of font_name in
  Draw.set_color ~io color;
  let x, y = Point.to_tuple at in
  set_snapped_font ~io m font_name size;
  let ctx = io.backend.ctx in
  let baseline = y +. Font_metrics.line_ascent m size in
  Clip.draw_clip ~io ctx (fun () ->
      C.fill_text ctx (Jstr.of_string text) ~x:(Float.round x) ~y:baseline)

let text_size ~io ?font ?size text =
  let font_name, size = get_font ~io font size in
  let w, h = Font_metrics.text_size (metrics_of font_name) size text in
  Size.v w h
