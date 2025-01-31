type opcode
type t

let create: opcode -> int -> int -> int -> t

val parse: bytes -> instruction
