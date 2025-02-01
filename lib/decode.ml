open Stdint

type instruction =
  MachineRoutine of int
| Clear
| Jump of uint16
| JumpOffset of uint16
| CallSubroutine of uint16
| Return
| SkipEqualN of Register.address * uint8
| SkipNotEqualN of Register.address * uint8
| SkipEqual of Register.address * Register.address
| SkipNotEqual of Register.address * Register.address
| SetN of Register.address * uint8
| AddN of Register.address * uint8
| SetToY of Register.address * Register.address
| OR of Register.address * Register.address
| AND of Register.address * Register.address
| XOR of Register.address * Register.address
| Add of Register.address * Register.address
| Sub of Register.address * Register.address
| RevSub of Register.address * Register.address
| ShiftLeft of Register.address * Register.address
| ShiftRight of Register.address * Register.address
| SetIndex of Register.address
| AddIndex of Register.address
| Random of int
| Display of Register.address * Register.address * int
| SkipKeyPressed of Register.address
| SkipKeyNotPressed of Register.address
| GetDelayTimer of Register.address
| SetDelayTimer of Register.address
| SetSoundTimer of Register.address
| GetKey of Register.address
| FontChar of Register.address
| BinToDec of Register.address
| WriteMemory of Register.address
| ReadMemory of Register.address

type decode_error =
  InvalidOpcode of int * uint16

let get_opcode bin =
  Uint16.to_int @@ Binary.first_nibble bin

let decode_instruction bin =
  let opcode = get_opcode bin in
  match opcode with
    0 -> Ok Clear
  | 1 -> Ok (Jump (Binary.to_NNN bin))
  | _ -> Error (InvalidOpcode (opcode, bin))
