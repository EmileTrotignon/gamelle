open Gamelle_common
open Geometry

type io_backend
type io = io_backend Gamelle_common.abstract_io

val run : 'state -> (io:io -> 'state -> 'state) -> unit

module Bitmap : sig
  type t

  val load : string -> t
  val sub : t -> int -> int -> int -> int -> t
end

module Font : sig
  type t

  val default : t
  val default_size : int
  val load : string -> t
  val set_font : t -> io_backend -> io_backend
  val set_font_size : int -> io_backend -> io_backend
end

module Sound : sig
  type data

  val play_until_end : io:io -> data -> unit
  val play_music : io:io -> data -> unit
  val stop_music : io:io -> unit
  val data_duration : data -> float

  type t

  val init : io:io -> data -> t
  val play : io:io -> t -> bool
  val play_loop : io:io -> t -> unit
  val time_left : t -> float
  val current_time : t -> float
  val duration : t -> float
  val load : string -> data
end

val clock : io:io -> float
val dt : io:io -> float
val draw : io:io -> Bitmap.t -> point -> unit
val draw_line : io:io -> ?color:Color.t -> Segment.t -> unit
val draw_rect : io:io -> ?color:Color.t -> box -> unit
val fill_rect : io:io -> ?color:Color.t -> box -> unit

(* [radius] is the corner radius, already clamped by the caller to at most half
   the box's smallest side. *)
val draw_rounded_rect : io:io -> ?color:Color.t -> radius:float -> box -> unit
val fill_rounded_rect : io:io -> ?color:Color.t -> radius:float -> box -> unit
val draw_poly : io:io -> ?color:Color.t -> Polygon.t -> unit
val fill_poly : io:io -> ?color:Color.t -> Polygon.t -> unit
val draw_circle : io:io -> ?color:Color.t -> Circle.t -> unit
val fill_circle : io:io -> ?color:Color.t -> Circle.t -> unit
val draw_arc : io:io -> ?color:Color.t -> Arc.t -> unit
val fill_arc : io:io -> ?color:Color.t -> Arc.t -> unit

module Text : sig
  type t

  val to_string : t -> string
  val of_string : string -> t
  val ( ^ ) : t -> t -> t
  val slice : ?start:int -> ?stop:int -> t -> t
  val length : t -> int
  val get : t -> int -> t
  val chars : t -> t list

  (* Draw a single glyph (a one-codepoint [t]) with its layout origin at [at].
     Inter-glyph advancing is done once, backend-agnostically, in [Draw_geometry]
     using [size]; each backend only has to place one glyph and query sizes. *)
  val draw_glyph :
    io:io ->
    ?color:Color.t ->
    ?font:Font.t ->
    ?size:int ->
    at:point ->
    t ->
    unit

  val size : io:io -> ?font:Font.t -> ?size:int -> t -> size
end

module Window : sig
  val show_cursor : io:io -> bool -> unit
  val set_fullscreen : io:io -> bool -> unit
  val get_fullscreen : io:io -> bool
  val size : io:io -> Size.t
end

module Net : sig
  (* A websocket connection. The transport runs asynchronously (a background
     domain running Lwt on native backends, the browser event loop on jsoo); the
     game reads and writes it non-blockingly, once per frame, via [poll] and
     [send]. *)
  type t

  type status =
    | Connecting  (* handshake in progress *)
    | Connected  (* open and usable *)
    | Closed  (* closed cleanly, by us or the server *)
    | Error of string  (* the connection failed; the string describes why *)

  val connect : string -> t
  val send : t -> string -> unit

  (* Messages received since the previous [poll], in arrival order. Never
     blocks. *)
  val poll : t -> string list

  (* Current state of the connection. Never blocks. *)
  val status : t -> status
  val is_connected : t -> bool
  val close : t -> unit
end
