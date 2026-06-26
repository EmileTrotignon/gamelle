(* Backend-agnostic text metrics and layout, shared by the raylib and browser
   backends so they size and position glyphs identically. Everything is read from
   the font's otfm tables and is pure (no GL/canvas), which is why it lives in
   common. The SDL backend ignores this and rasterises through SDL_ttf instead.

   Both backends render at the same integer pixel height [H = round (size *.
   em_scale)] and advance the pen by the font's exact per-glyph [hmtx] advances
   (no kerning), so their glyph positions match byte-for-byte. *)

type t = {
  em_scale : float;  (** (ascent - descent) / units_per_em. *)
  asc_ratio : float;  (** ascent / units_per_em. *)
  desc_ratio : float;  (** -descent / units_per_em. *)
  advance : (int, float) Hashtbl.t;  (** advance in em units, by codepoint. *)
  default_advance : float;
      (** advance of .notdef, used for unmapped codepoints. *)
}

(* A font failing to decode means it is either corrupt or uses a feature otfm
   doesn't support (a .ttc collection, or a cmap subtable that isn't format 4 /
   12 / 13). Rather than silently rendering wrong-sized text, fail loudly so the
   font can be fixed. *)
let fail table e =
  Format.kasprintf failwith "gamelle: cannot read the %s table of the font: %a"
    table Otfm.pp_error e

let of_ttf data =
  let d = Otfm.decoder (`String data) in
  let upm =
    match Otfm.head d with
    | Ok h ->
        if h.Otfm.head_units_per_em > 0 then h.Otfm.head_units_per_em else 1000
    | Error e -> fail "head" e
  in
  let upm = float_of_int upm in
  let asc, desc =
    match Otfm.hhea d with
    | Ok hh ->
        ( float_of_int hh.Otfm.hhea_ascender /. upm,
          float_of_int (-hh.Otfm.hhea_descender) /. upm )
    | Error e -> fail "hhea" e
  in
  let n =
    match Otfm.glyph_count d with Ok n -> n | Error e -> fail "maxp" e
  in
  let glyph_advance = Array.make (max 1 n) 0 in
  begin match
    Otfm.hmtx d
      (fun () gid adv _lsb ->
        if gid >= 0 && gid < n then glyph_advance.(gid) <- adv)
      ()
  with
  | Ok () -> ()
  | Error e -> fail "hmtx" e
  end;
  let default_advance =
    if n > 0 then float_of_int glyph_advance.(0) /. upm else 0.6
  in
  let advance = Hashtbl.create 512 in
  (match
     Otfm.cmap d
       (fun () kind (u0, u1) gid ->
         (* Bound the work in case of a pathologically large range. *)
         let u1 = min u1 (u0 + 0xFFFF) in
         for u = u0 to u1 do
           let g =
             match kind with `Glyph -> gid | `Glyph_range -> gid + (u - u0)
           in
           if g >= 0 && g < n then
             Hashtbl.replace advance u (float_of_int glyph_advance.(g) /. upm)
         done)
       ()
   with
  | Ok _ -> ()
  | Error e -> fail "cmap" e);
  {
    em_scale = asc +. desc;
    asc_ratio = asc;
    desc_ratio = desc;
    advance;
    default_advance;
  }

(** Exact advance of [cp], in em units (the font's .notdef advance if unmapped).
*)
let advance_em t cp =
  match Hashtbl.find_opt t.advance cp with
  | Some a -> a
  | None -> t.default_advance

let iter_codepoints text f =
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    let d = String.get_utf_8_uchar text !i in
    f (Uchar.to_int (Uchar.utf_decode_uchar d));
    i := !i + Uchar.utf_decode_length d
  done

let string_of_cp cp =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int cp);
  Buffer.contents b

(* Integer pixel height the glyph atlas / css font is rendered at. *)
let pixel_height t size =
  int_of_float (Float.round (float_of_int size *. t.em_scale))

(* Pixels per em at [size]: one em is this many pixels. Advances, the baseline and
   the line height are all scaled by it. *)
let em_px t size = float_of_int (pixel_height t size) /. t.em_scale

(* Baseline offset and line height, ceiling each component to whole pixels (as the
   browser does with fontBoundingBoxAscent / Descent), so lines line up. *)
let line_ascent t size = Float.ceil (t.asc_ratio *. em_px t size)

let line_height t size =
  line_ascent t size +. Float.ceil (t.desc_ratio *. em_px t size)

(* The font's own ascent in pixels (un-ceiled): where a glyph's baseline sits
   relative to the top of its drawn cell. *)
let raw_ascent t size = t.asc_ratio *. em_px t size

(* Total advance width of [text] at [size] (un-snapped). *)
let text_width t size text =
  let em = em_px t size in
  let w = ref 0.0 in
  iter_codepoints text (fun cp -> w := !w +. (advance_em t cp *. em));
  !w

(* Rendered (width, height) of [text] at [size]. *)
let text_size t size text = (text_width t size text, line_height t size)
