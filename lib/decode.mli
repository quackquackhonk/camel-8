open Stdint

type instruction
type decode_error

val decode_instruction : uint16 -> (instruction, decode_error) result
