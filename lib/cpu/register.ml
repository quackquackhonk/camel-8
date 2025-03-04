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

let pretty bank =
  let open Printf in
  let regs = [Hex.Zero; Hex.One; Hex.Two; Hex.Three; Hex.Four;
              Hex.Five; Hex.Six; Hex.Seven; Hex.Eight; Hex.Nine;
              Hex.A; Hex.B; Hex.C; Hex.D; Hex.E; Hex.F]
  in
  let fmt a =
    let v = if HexMap.mem a bank
            then HexMap.find a bank
            else Uint8.zero
    in
    let s = Pretty.uint8_to_bin_string v in
    sprintf "v%s: %s" (Hex.to_string a) s
  in
  List.map fmt regs
