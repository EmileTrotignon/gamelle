open Draw_geometry

type t

val v :
  ?min_width:float ->
  ?flex_width:float ->
  ?max_width:float ->
  ?min_height:float ->
  ?flex_height:float ->
  ?max_height:float ->
  (Box.t -> unit) ->
  t

val fixed : ?width:float -> ?height:float -> (Box.t -> unit) -> t

type h

val height : ?min:float -> ?max:float -> ?flex:float -> (Box.t -> unit) -> h
val width : ?min:float -> ?max:float -> ?flex:float -> (float -> h) -> t

(* *)

val horizontal : ?gap:float -> t list -> t
val vertical : ?gap:float -> t list -> t
val over : t list -> t
val vclip : float -> t -> t
val hclip : float -> t -> t
val pad : float -> t -> t
val center : t list -> t
val solve : at:Point.t -> ?size:(Size.t -> Size.t) -> t -> Size.t

type constrain = { min : float; flex : float; max : float }

val reshape :
  ?width:(constrain -> constrain) -> ?height:(constrain -> constrain) -> t -> t
