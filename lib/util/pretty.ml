open Stdint

let uint16_to_hex_string n =
  let open Binary in
  let open Hex in
  let convert f n = f n |> Uint16.to_int |> of_int |> to_string in
  Printf.sprintf "0x%s%s%s%s"
    (convert first_nibble n)
    (convert second_nibble n)
    (convert third_nibble n)
    (convert fourth_nibble n)

let uint16_to_bin_string = ""

let uint8_to_bin_string = ""
