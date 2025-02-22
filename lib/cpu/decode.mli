open Stdint

type cond = EqN of uint8
          | NEqN of uint8
          | EqR of Register.address
          | NEqR of Register.address

type binop = Add
           | Sub
           | OR
           | AND
           | XOR
           | ShiftLeft
           | ShiftRight

type instruction =
  MachineRoutine of int
| Clear
| Jump of uint16
| JumpOffset of uint16
| CallSubroutine of uint16
| Return
| If of Register.address * cond
| SetN of Register.address * uint8
| AddN of Register.address * uint8
| SetXY of Register.address * Register.address
| Binop of binop * Register.address * Register.address
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
