open! Brr
module C = Brr_canvas.C2d

(* SVGs are drawn like bitmaps: the browser rasterises the vector source itself,
   at whatever resolution the canvas draw call needs, so it stays crisp under
   any view scale. The source is handed to an [Image] through a
   [data:image/svg+xml] URL, exactly as [Bitmap] does for PNG/JPEG. [w] and [h]
   are the SVG's intrinsic dimensions (from the asset packer); the image is
   drawn at that logical size, which the view transform then scales/rotates. *)

type t = {
  image : C.image_src;
  backend : Jv.t;
  w : int;
  h : int;
  mutable error : bool;
}

let class_image = Jv.get Jv.global "Image"
let new_image () = Jv.new' class_image [||]

let tarray_of_string binstring =
  let len = String.length binstring in
  let arr = Tarray.create Tarray.Uint8 len |> Brr.Tarray.to_bigarray1 in
  for i = 0 to len - 1 do
    Bigarray.Array1.set arr i (Char.code binstring.[i])
  done;
  arr

let load ~w ~h svg =
  let img = tarray_of_string svg in
  let b64 =
    Base64.data_of_binary_jstr
    @@ Tarray.to_binary_jstr (Tarray.of_bigarray1 img)
  in
  let b64 =
    match Base64.encode b64 with
    | Ok v -> v
    | Error e ->
        Console.(log [ "b64"; e ]);
        failwith "base64 encode"
  in
  let url =
    Jstr.of_string ("data:image/svg+xml;base64," ^ Jstr.to_string b64)
  in
  let image = new_image () in
  let t =
    { image = C.image_src_of_jv image; backend = image; w; h; error = false }
  in
  Jv.set image "onerror" (Jv.callback ~arity:1 (fun _v -> t.error <- true));
  Jv.set image "src" (Jv.of_jstr url);
  t

let is_complete t = (not t.error) && Jv.to_bool (Jv.get t.backend "complete")

let draw ~io:_ ~ctx t ~x ~y =
  if is_complete t then
    C.draw_image_in_rect ctx t.image ~x ~y ~w:(float t.w) ~h:(float t.h)
