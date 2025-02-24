open Stdint

type t = uint64 array

val create : unit -> t

val draw : t -> x:uint8 -> y:uint8 -> s:uint8 -> n:int -> t * bool
(** [draw d x y s n] draws the sprite [s], [n] pixels tall onto the display [d]
    at position [x] x [y], returning the new display and a flag indicating any
    bits turned off.*)

val to_string : t -> string
