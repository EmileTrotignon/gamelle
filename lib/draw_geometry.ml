open Gamelle_common
open Gamelle_backend

type xy = Geometry.Xy.t = { x : float; y : float }

module Point = struct
  include Geometry.Point

  let draw ~io ?color pt = z ~io (fill_circle ?color (Geometry.Circle.v pt 3.0))
end

module Polygon = struct
  include Geometry.Polygon

  let draw ~io ?color s = z ~io (draw_poly ?color s)
  let fill ~io ?color s = z ~io (fill_poly ?color s)
end

module Shape = struct
  include Geometry.Shape

  let draw ~io ?color = function
    | Segment s -> draw_line ~io ?color s
    | Circle c -> draw_circle ~io ?color c
    | Polygon pts -> draw_poly ~io ?color pts

  let draw ~io ?color s = z ~io (draw ?color s)

  let fill ~io ?color = function
    | Segment s -> draw_line ~io ?color s
    | Circle c -> fill_circle ~io ?color c
    | Polygon pts -> fill_poly ~io ?color pts

  let fill ~io ?color s = z ~io (fill ?color s)
end

module Vec = struct
  include Geometry.Vec

  let pi = 4.0 *. atan 1.0

  let draw ~io ?color ~at vec =
    Point.draw ~io ?color at;
    z ~io (fun ~io ->
        let stop = at + vec in
        draw_line ~io ?color (Geometry.Segment.v at stop);
        let stop1 = stop + (10. * unit vec) in
        let angle = 90. /. pi in
        let cos = cos angle and sin = sin angle in
        let arrow = Point.rotate_around ~angle:(cos, sin) ~center:stop stop1 in
        draw_line ~io ?color (Geometry.Segment.v stop arrow);
        let arrow =
          Point.rotate_around ~angle:(cos, -.sin) ~center:stop stop1
        in
        draw_line ~io ?color (Geometry.Segment.v stop arrow))
end

module Color = struct
  include Geometry.Color
end

module Segment = struct
  include Geometry.Segment

  let draw ~io ?color s = z ~io (draw_line ?color s)
end

module Circle = struct
  include Geometry.Circle

  let draw ~io ?color c = z ~io (draw_circle ?color c)
  let fill ~io ?color c = z ~io (fill_circle ?color c)
end

module Arc = struct
  include Geometry.Arc

  let draw ~io ?color a = z ~io (draw_arc ?color a)

  (* Filling an arc draws the corresponding circular sector (pie slice): the
     curve closed back to the center of the circle. *)
  let fill ~io ?color a = z ~io (fill_arc ?color a)
end

module Box = struct
  include Geometry.Box

  let draw ~io ?color r = z ~io (draw_rect ?color r)
  let fill ~io ?color r = z ~io (fill_rect ?color r)

  (* Clamp the corner radius to at most half the smallest side, so it never
     self-overlaps, before handing it to the backend's rounded-rectangle
     primitive. *)
  let clamp_radius ~radius box =
    Float.max 0.0 (Float.min radius (0.5 *. Float.min (width box) (height box)))

  let draw_rounded ~io ?color ~radius box =
    z ~io (draw_rounded_rect ?color ~radius:(clamp_radius ~radius box) box)

  let fill_rounded ~io ?color ~radius box =
    z ~io (fill_rounded_rect ?color ~radius:(clamp_radius ~radius box) box)
end

module Size = struct
  include Geometry.Size

  let draw ~io ?color ~at t = Box.draw ~io ?color (Box.v at t)
  let fill ~io ?color ~at t = Box.fill ~io ?color (Box.v at t)
end

type color = Geometry.color
type point = Geometry.point
type vec = Geometry.vec
type segment = Geometry.segment
type circle = Geometry.circle
type arc = Geometry.arc
type box = Geometry.box
type size = Geometry.size
type polygon = Polygon.t
type shape = Shape.t

module Text = struct
  include Gamelle_backend.Text

  let sub txt i l = slice ~start:i ~stop:(i + l) txt

  let split_on_char char t =
    t |> to_string |> String.split_on_char char |> List.map of_string

  let concat li =
    let buffer = Buffer.create 128 in
    List.iter (fun str -> Buffer.add_string buffer (to_string str)) li;
    of_string (Buffer.contents buffer)

  (* Backend-agnostic single-line layout: place each glyph at the running pen
     position (snapped to a whole pixel) and advance by the glyph's measured
     width. Each backend only implements [draw_glyph] (one glyph) and [size]. *)
  let draw ~io ?color ?font ?size ~at t =
    let s = to_string t in
    let x0 = Geometry.Point.x at and y0 = Geometry.Point.y at in
    let lx = ref 0.0 in
    Font_metrics.iter_codepoints s begin fun cp ->
        let glyph = of_string (Font_metrics.string_of_cp cp) in
        let at = Geometry.Point.v (Float.round (x0 +. !lx)) y0 in
        draw_glyph ~io ?color ?font ?size ~at glyph;
        lx :=
          !lx
          +. Geometry.Size.width
               (Gamelle_backend.Text.size ~io ?font ?size glyph)
      end

  let draw_t = draw
  let size_t = size
  let size ~io ?font ?size t = size_t ~io ?font ?size (of_string t)

  let min_size ~io ?font ?size str =
    let words = String.split_on_char ' ' str in
    List.fold_left
      begin fun (min_width, min_height) word ->
        let s = size_t ~io ?font ?size (of_string word) in
        (max min_width (Size.width s), max min_height (Size.height s))
      end
      (0.0, 0.0) words

  let size_multiline_t ~io ?(width = Float.infinity) ?(interline = -8.) ?font
      ?size (text : t) =
    let pos = Vec.zero in
    let text_size = Gamelle_backend.Text.size ~io ?font ?size in
    let limx = width +. pos.x in
    let startx = pos.x in
    let lines = split_on_char '\n' text in
    let hline = Size.height (text_size (Text.of_string "a")) +. interline in
    let udpate_cpos maxw cpos = (Float.max maxw (Size.width cpos), cpos) in
    let print_line (maxw, cpos) line =
      let words = split_on_char ' ' line in
      let print_word (maxw, cpos) word =
        let size = text_size (word ^ of_string " ") in
        let w = Size.width size in
        let split_word word cpos =
          let chars = chars word in
          List.fold_left
            (fun (maxw, cpos) char ->
              let size = text_size char in
              let w = Size.width size in
              let cpos =
                if w +. cpos.x >= limx then Point.v startx (cpos.y +. hline)
                else cpos
              in
              udpate_cpos maxw Vec.(cpos + v w 0.))
            (maxw, cpos) chars
        in
        let maxw, cpos =
          if w >= width then split_word word cpos
          else if w +. cpos.x >= limx then
            let cpos = Point.v startx (cpos.y +. hline) in
            (maxw, cpos)
          else (maxw, cpos)
        in
        udpate_cpos maxw Vec.(cpos + v w 0.)
      in

      let maxw, cpos = List.fold_left print_word (maxw, cpos) words in
      (maxw, Point.v startx (cpos.y +. hline))
    in
    let maxw, end_pos = List.fold_left print_line (0., pos) lines in
    (* [end_pos.y] advanced by a full [hline] (line height + interline) past the
       last line's top. The bounding box only needs the last line's actual height
       there, not another interline gap, so drop the trailing [interline] —
       otherwise (with the default negative [interline]) the box is too short and
       the last line's descenders spill out below it. *)
    Size.v maxw (end_pos.y -. interline)

  let size_multiline ~io ?width ?interline ?font ?size str =
    size_multiline_t ~io ?width ?interline ?font ?size (of_string str)

  let draw_multiline_t ~io ?(width = Float.infinity) ?(interline = -8.) ?font
      ?color ?size ~at:pos text =
    let text_size = Gamelle_backend.Text.size ~io ?font ?size in
    let draw_string = draw_t ~io ?color ?size ?font in
    let limx = width +. pos.x in
    let startx = pos.x in
    let lines = split_on_char '\n' text in
    let hline = Size.height (text_size (of_string "a")) +. interline in
    let print_line cpos line =
      let words = split_on_char ' ' line in
      let print_word cpos word =
        let size = text_size (word ^ of_string " ") in
        let w = Size.width size in
        let split_word word cpos =
          let chars = chars word in
          List.fold_left
            begin fun cpos char ->
              let size = text_size char in
              let w = Size.width size in
              let cpos =
                if w +. cpos.x >= limx then Point.v startx (cpos.y +. hline)
                else cpos
              in
              draw_string char ~at:cpos;
              Vec.(cpos + v w 0.)
            end
            cpos chars
        in
        let cpos =
          if w >= width then split_word word cpos
          else if w +. cpos.x >= limx then (
            let cpos = Point.v startx (cpos.y +. hline) in
            draw_string word ~at:cpos;
            cpos)
          else (
            draw_string (word ^ of_string " ") ~at:cpos;
            cpos)
        in
        Vec.(cpos + v w 0.)
      in
      let cpos = List.fold_left print_word cpos words in
      Point.v startx (cpos.y +. hline)
    in
    ignore (List.fold_left print_line pos lines);
    ()

  let draw_t ~io ?color ?font ?size ~at t =
    z ~io (draw ?color ?font ?size ~at t)

  let draw_multiline_t ~io ?color ?width ?interline ?font ?size ~at t =
    z ~io (draw_multiline_t ?color ?width ?interline ?font ?size ~at t)

  let draw_multiline ~io ?color ?width ?interline ?font ?size ~at str =
    draw_multiline_t ~io ?width ?interline ?font ?color ?size ~at
      (of_string str)

  let draw ~io ?color ?font ?size ~at t =
    draw_t ~io ?color ?font ?size ~at (of_string t)
end

let draw = Bitmap_.draw
