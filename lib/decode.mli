open Stdint

type instruction =
  MachineRoutine of int
| Clear
| Jump of uint16
| JumpOffset of uint16
| CallSubroutine of uint16
| Return
| IfEqualXN of Register.address * uint8
| IfNotEqualXN of Register.address * uint8
| IfEqualXY of Register.address * Register.address
| IfNotEqualXY of Register.address * Register.address
| SetN of Register.address * uint8
| AddN of Register.address * uint8
| SetXY of Register.address * Register.address
| OR of Register.address * Register.address
| AND of Register.address * Register.address
| XOR of Register.address * Register.address
| Add of Register.address * Register.address
| Sub of Register.address * Register.address
| ShiftLeft of Register.address * Register.address
| ShiftRight of Register.address * Register.address
| SetIndex of uint16
| AddIndex of Register.address
| Random of Register.address * uint8
| Display of Register.address * Register.address * int
| IfKeyPressed of Register.address
| IfKeyNotPressed of Register.address
| GetDelayTimer of Register.address
| SetDelayTimer of Register.address
| SetSoundTimer of Register.address
| GetKey of Register.address
| FontChar of Register.address
| BinToDec of Register.address
| WriteMemory of Register.address
| ReadMemory of Register.address

exception Decode_error of string

val decode : uint16 -> instruction
