include Gg.Color

(* Convert sRGB component (0–255 int) to linear light for Gg.Color storage.
   to_srgbi applies gamma on output, so we pre-linearize here so that
   rgb 255 0 0 renders as #FF0000, rgb 40 40 40 renders as #282828, etc. *)
let srgb_to_linear x =
  let x = float x /. 255.0 in
  if x <= 0.04045 then x /. 12.92 else ((x +. 0.055) /. 1.055) ** 2.4

let rgb ?(alpha = 1.0) r g b =
  v (srgb_to_linear r) (srgb_to_linear g) (srgb_to_linear b) alpha

let hsl ?alpha h s l = Color.Hsl.(v ?alpha h s l |> to_gg)
let with_alpha alpha t = with_a t alpha
let yellow = rgb 255 255 0
let cyan = rgb 0 255 255
let magenta = rgb 255 0 255
let gray = rgb 128 128 128
let purple = rgb 128 0 255
let orange = rgb 255 128 0
let pink = rgb 255 105 180
let teal = rgb 0 128 128
let coral = rgb 255 127 80
let gold = rgb 255 215 0
let violet = rgb 238 130 238
let crimson = rgb 220 20 60
let lime = rgb 50 205 50
let indigo = rgb 75 0 130
let turquoise = rgb 64 224 208
let brown = rgb 165 42 42
let silver = rgb 192 192 192
