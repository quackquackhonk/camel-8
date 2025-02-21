open Stdint

val uint16_to_bytes : uint16 -> bytes -> int -> unit
(** [uint16_to_bytes] [i] [buffer] [offset] writes the integer [i] to the [buffer] [buffer] starting at [offset] [offset].
    The byte order used is the system's native endian. If the [buffer] does not hold enough bytes,
    i.e. if (Bytes.length [buffer]) < ([offset] + (bits / 8)), the function
    will raise Invalid_argument "index out of bounds". *)

val first_nibble  : uint16 -> uint16
(** [first_nibble] [bin] returns the first 4 bits of [bin], shifted right into the least significant bits.*)
val second_nibble : uint16 -> uint16
(** [second_nibble] [bin] returns the second 4 bits of [bin], shifted right into the least significant bits.*)
val third_nibble  : uint16 -> uint16
(** [third_nibble] [bin] returns the third 4 bits of [bin], shifted right into the least significant bits.*)
val fourth_nibble : uint16 -> uint16
(** [fourth_nibble] [bin] returns the last 4 bits of [bin].*)
