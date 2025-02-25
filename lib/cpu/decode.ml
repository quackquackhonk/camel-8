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

type error =
  InvalidNN of int * int * uint16
| InvalidBinop of int * uint16

exception Decode_error of string

let fmt_err e =
  match e with
    InvalidNN (op, code, bin) -> Printf.sprintf "%x: %x is not a valid code for opcode %x" (Uint16.to_int bin) code op
  | InvalidBinop (sub, bin) -> Printf.sprintf "%x: %x is not a valid subcode for binops" (Uint16.to_int bin) sub


let get_opcode bin = Binary.first_nibble bin |> Uint16.to_int
let get_n bin = Binary.fourth_nibble bin |> Uint16.to_int
let get_nn bin =
  let mask = Uint16.of_int 0x00FF in
  Uint16.logand mask bin |> Uint8.of_uint16
let get_nnn bin =
  let mask = Uint16.of_int 0x0FFF in
  Uint16.logand mask bin

let decode_res bin =
  let opcode = get_opcode bin in
  let x = Binary.second_nibble bin |> Uint16.to_int |> Hex.of_int in
  let y = Binary.third_nibble bin |> Uint16.to_int |> Hex.of_int in
  let n = get_n bin in
  let nnn = get_nnn bin in
  let nn = get_nn bin in
  match (opcode, Uint8.to_int nn, n) with
  | (0x0, 0xE0, _) -> Ok Clear
  | (0x0, 0xEE, _) -> Ok Return
  | (0x0, _, _) -> Error (InvalidNN (opcode, n, bin))
  | (0x1, _, _) -> Ok (Jump (nnn))
  | (0x2, _, _) -> Ok (CallSubroutine (nnn))
  | (0x3, _, _) -> Ok (If (x, NEqN nn))
  | (0x4, _, _) -> Ok (If (x, EqN nn))
  | (0x5, _, _) -> Ok (If (x, NEqR y))
  | (0x6, _, _) -> Ok (SetN (x, nn))
  | (0x7, _, _) -> Ok (AddN (x, nn))
  (* Binary instruction *)
  | (0x8, _, 0x0) -> Ok (SetXY (x, y))
  | (0x8, _, 0x1) -> Ok (Binop (OR, x, y))
  | (0x8, _, 0x2) -> Ok (Binop (AND, x, y))
  | (0x8, _, 0x3) -> Ok (Binop (XOR, x, y))
  | (0x8, _, 0x4) -> Ok (Binop (Add, x, y))
  | (0x8, _, 0x5) -> Ok (Binop (Sub, x, y))
  | (0x8, _, 0x6) -> Ok (Binop (ShiftRight, x, y))
  | (0x8, _, 0x7) -> Ok (Binop (Sub, y, x))
  | (0x8, _, 0xE) -> Ok (Binop (ShiftLeft, x, y))
  | (0x8, _, _) -> Error (InvalidBinop (n, bin))
  | (0x9, _, _) -> Ok (If (x, EqR y))
  | (0xA, _, _) -> Ok (SetIndex (nnn))
  | (0xB, _, _) -> Ok (JumpOffset (nnn))
  | (0xC, _, _) -> Ok (Random (x, nn))
  | (0xD, _, _) -> Ok (Display (x, y, n))
  (* Keycode instructions *)
  | (0xE, 0x9E, _) -> Ok (IfKeyPressed (x))
  | (0xE, 0xA1, _) -> Ok (IfKeyNotPressed (x))
  | (0xE, nn, _) -> Error (InvalidNN (opcode, nn, bin))
  (* Special / Misc instructions *)
  | (0xF, 0x07, _) -> Ok (GetDelayTimer (x))
  | (0xF, 0x15, _) -> Ok (SetDelayTimer (x))
  | (0xF, 0x18, _) -> Ok (SetSoundTimer (x))
  | (0xF, 0x1E, _) -> Ok (AddIndex (x))
  | (0xF, 0x0A, _) -> Ok (GetKey (x))
  | (0xF, 0x29, _) -> Ok (FontChar (x))
  | (0xF, 0x33, _) -> Ok (BinToDec (x))
  | (0xF, 0x55, _) -> Ok (WriteMemory (x))
  | (0xF, 0x65, _) -> Ok (ReadMemory (x))
  | (0xF, nn, _) -> Error (InvalidNN (opcode, nn, bin))
  | _ -> Error (InvalidNN (opcode, n, bin))

let decode bin =
  match decode_res bin with
    Ok i -> i
  | Error e -> raise (Decode_error (fmt_err e))
