open Stdint

type t = uint64 array

val create : unit -> t

val draw : t -> x:uint8 -> y:uint8 -> sprite:uint8 list -> t * bool
(** [draw d ~x ~y ~sprite] draws [sprite] onto the display [d]
    at position [x] x [y], returning the new display and a flag indicating any
    bits turned off.*)

val to_bool_array : t -> bool array array

val to_string : t -> string
