open Stdint

type error

exception Disassemble_error of string

val disassemble : string -> uint16 array
