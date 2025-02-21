open Stdint

type t

exception Memory_error of string

val memory_size : int
val instruction_start : int

val create : bytes -> t

include Addressable_intf.ByteS with type t := t
include Addressable_intf.WordS with type t := t
