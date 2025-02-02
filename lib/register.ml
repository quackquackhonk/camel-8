open Stdint

type address = int
type bank = uint16 array

let num_registers = 16

let init_bank _ = Array.make num_registers Uint16.zero
