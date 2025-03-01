open Stdint

type address = Hex.t
type bank

val num_registers : int

val init_bank : unit -> bank

val set : bank -> reg:address -> data:uint8 -> bank
val get : bank -> reg:address -> uint8

val set_carry : bank -> bool -> bank
val get_carry : bank -> bool

val pretty : bank -> string list
