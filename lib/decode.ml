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

type decode_error =
  Invalid of int * int * uint16
| InvalidSubcode of int * int * uint16

let get_opcode bin = Binary.first_nibble bin |> Uint16.to_int
let get_n bin = Binary.fourth_nibble bin |> Uint16.to_int
let get_nn bin =
  let mask = Uint16.of_int 0x00FF in
  Uint16.logand mask bin |> Uint8.of_uint16
let get_nnn bin =
  let mask = Uint16.of_int 0x0FFF in
  Uint16.logand mask bin

let decode_instruction bin =
  let opcode = get_opcode bin in
  let x = Uint16.to_int @@ Binary.second_nibble bin in
  let y = Uint16.to_int @@ Binary.third_nibble bin in
  let nnn = get_nnn bin in
  let nn = get_nn bin in
  let subcode = get_n bin in
  match (opcode, subcode) with
    (0x0, 0x0) -> Ok Clear
  | (0x0, 0xE) -> Ok Return
  | (0x0, _) -> Error (Invalid (opcode, subcode, bin))
  | (0x1, _) -> Ok (Jump (nnn))
  | (0x2, _) -> Ok (CallSubroutine (nnn))
  | (0x3, _) -> Ok (IfNotEqualXN (x, nn))
  | (0x4, _) -> Ok (IfEqualXN (x, nn))
  | (0x5, _) -> Ok (IfNotEqualXY (x, y))
  | (0x6, _) -> Ok (SetN (x, nn))
  | (0x7, _) -> Ok (AddN (x, nn))
  (* Binary instruction *)
  | (0x8, 0x0) -> Ok (SetXY (x, y))
  | (0x8, 0x1) -> Ok (OR (x, y))
  | (0x8, 0x2) -> Ok (AND (x, y))
  | (0x8, 0x3) -> Ok (XOR (x, y))
  | (0x8, 0x4) -> Ok (Add (x, y))
  | (0x8, 0x5) -> Ok (Sub (x, y))
  | (0x8, 0x6) -> Ok (ShiftRight (x, y))
  | (0x8, 0x7) -> Ok (Sub (y, x))
  | (0x8, 0xE) -> Ok (ShiftLeft (x, y))
  | (0x8, _) -> Error (InvalidSubcode (opcode, subcode, bin))
  | (0x9, _) -> Ok (IfEqualXY (x, y))

  | (0xA, _) -> Ok (SetIndex (nnn))
  | (0xB, _) -> Ok (JumpOffset (nnn))
  | (0xC, _) -> Ok (Random (x, nn))
  | (0xD, _) -> Ok (Display (x, y, subcode))
  (* Keycode instructions *)
  | (0xE, _) -> begin
      match Uint8.to_int nn with
        0x9E -> Ok (IfKeyPressed (x))
      | 0xA1 -> Ok (IfKeyNotPressed (x))
      | nn -> Error (Invalid (opcode, nn, bin))
      end
  (* Special / Misc instructions *)
  | (0xF, _) -> begin
      match Uint8.to_int nn with
        0x07 -> Ok (GetDelayTimer (x))
      | 0x15 -> Ok (SetDelayTimer (x))
      | 0x18 -> Ok (SetSoundTimer (x))
      | 0x1E -> Ok (AddIndex (x))
      | 0x0A -> Ok (GetKey (x))
      | 0x29 -> Ok (FontChar (x))
      | 0x33 -> Ok (BinToDec (x))
      | 0x55 -> Ok (WriteMemory (x))
      | 0x65 -> Ok (ReadMemory (x))
      | nn -> Error (Invalid (opcode, nn, bin))
    end
  | _ -> Error (Invalid (opcode, subcode, bin))
