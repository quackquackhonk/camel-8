open Stdint

type t = {
    regs: Register.bank;
    pc: uint16;
    index: uint16;
    stack: uint16 list;
    delay_timer: uint8;
    sound_timer: uint8;
  }

val create : unit -> t

val binop : t -> Decode.binop -> x:Register.address -> y:Register.address -> t

val incr_pc : t -> t
val set_pc : t -> uint16 -> t

(* Register operations*)
val get_register : t -> reg:Hex.t -> Stdint.uint8
val set_register : t -> reg:Hex.t -> data:Stdint.uint8 -> t
val set_carry : t -> bool -> t

val get_index : t -> Stdint.uint16
val set_index : t -> data:Stdint.uint16 -> t

val get_delay : t -> Stdint.uint8
val set_delay : t -> data:Stdint.uint8 -> t

val set_sound : t -> data:Stdint.uint8 -> t

val stack_push : t -> uint16 -> t
val stack_pop : t -> uint16 * t

val pretty_stack : ?max_addrs:int -> t -> string list
