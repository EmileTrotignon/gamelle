module B = Gamelle_backend.Svg

type t = { w : int; h : int; svg : B.t }

let width t = t.w
let height t = t.h
let size t = Gamelle_common.Geometry.Size.v (float t.w) (float t.h)
let load ~w ~h data = { w; h; svg = B.load ~w ~h data }

let draw ~io ~at { svg; _ } =
  Gamelle_common.z ~io (Gamelle_backend.draw_svg svg at)
