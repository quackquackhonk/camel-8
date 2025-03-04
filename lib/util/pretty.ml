open Stdint
open Printf

let uint16_to_hex_string n =
  let open Binary in
  let open Hex in
  let convert f n = f n |> Uint16.to_int |> of_int |> to_string in
  Printf.sprintf "0x%s%s%s%s"
    (convert first_nibble n)
    (convert second_nibble n)
    (convert third_nibble n)
    (convert fourth_nibble n)

let explode_uint8 v =
  let lsb x = Uint8.(logand one x) in
  let f x = let check = Uint8.shift_right_logical v x in
            let check = lsb check in
            Uint8.(check = one)
  in
  List.init Uint8.bits f

let uint8_to_bin_string v =
  explode_uint8 v
  |> List.map (fun b -> if b then "1" else "0")
  |> String.concat ""
  |> sprintf "0b%s"

let explode_uint16 v =
  let lsb x = Uint16.(logand one x) in
  let f x = let check = Uint16.shift_right_logical v x in
            let check = lsb check in
            Uint16.(check = one)
  in
  List.init Uint16.bits f

let uint16_to_bin_string v =
  explode_uint16 v
  |> List.map (fun b -> if b then "1" else "0")
  |> String.concat ""
  |> sprintf "0b%s"
