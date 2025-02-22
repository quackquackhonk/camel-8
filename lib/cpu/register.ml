open Stdint

type address = Hex.t
module HexMap = Map.Make(Hex)
type bank = uint8 HexMap.t

let num_registers = 16

let init_bank _ = HexMap.empty

let set bank ~reg ~data = HexMap.add reg data bank
let get bank ~reg =
  match HexMap.find_opt reg bank with
    Some n -> n
  | None -> Uint8.zero

let set_carry bank carry =
  let data = if carry then Uint8.one else Uint8.zero in
  set bank ~reg:Hex.F ~data
let get_carry bank = Uint8.zero <> get bank ~reg:Hex.F
