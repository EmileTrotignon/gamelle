(** Backend-agnostic text metrics and layout, shared by the raylib and browser
    backends so they size and position glyphs identically. Read from the font's
    otfm tables; pure (no GL/canvas).

    Both backends render at the same integer pixel height
    [H = round (size *. em_scale)] and advance the pen by the font's exact
    per-glyph [hmtx] advances (no kerning), so their glyph positions match
    byte-for-byte. *)

type t
(** Sizing and advance metrics of a font. *)

val of_ttf : string -> t
(** [of_ttf data] reads the metrics from the TTF/OTF bytes [data]. Raises
    [Failure] with a descriptive message if the font is corrupt or uses a
    feature otfm doesn't support (a [.ttc] collection, or a cmap subtable that
    isn't format 4 / 12 / 13). *)

val iter_codepoints : string -> (int -> unit) -> unit
(** [iter_codepoints text f] calls [f] on each UTF-8 codepoint of [text]. *)

val string_of_cp : int -> string
(** [string_of_cp cp] is the single-codepoint UTF-8 string for [cp]. *)

val pixel_height : t -> int -> int
(** [pixel_height t size] is the integer pixel height the glyph atlas / css font
    is rendered at for the requested em [size]. *)

val em_px : t -> int -> float
(** [em_px t size] is the number of pixels in one em at [size]. Advances, the
    baseline and the line height are all scaled by it. *)

val line_ascent : t -> int -> float
(** [line_ascent t size] is the baseline offset from the top of the line (the
    line ascent, ceiled to a whole pixel). *)

val raw_ascent : t -> int -> float
(** [raw_ascent t size] is the font's own (un-ceiled) ascent in pixels: where a
    glyph's baseline sits relative to the top of its drawn cell. *)

val text_size : t -> int -> string -> float * float
(** [text_size t size text] is the rendered [(width, height)] of [text]. *)
