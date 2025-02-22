open Stdint

let nibbles = [|
    Uint16.of_int 0xF000;
    Uint16.of_int 0x0F00;
    Uint16.of_int 0x00F0;
    Uint16.of_int 0x000F;
  |]


let get_nibble nib bin =
  let unshifted = Uint16.logand bin nibbles.(nib) in
  let shift = Uint16.bits - (4 * (nib + 1)) in
  Uint16.shift_right_logical unshifted shift

let uint16_to_bytes = if Sys.big_endian
                      then Uint16.to_bytes_big_endian
                      else Uint16.to_bytes_little_endian

let first_nibble  = get_nibble 0
let second_nibble = get_nibble 1
let third_nibble  = get_nibble 2
let fourth_nibble = get_nibble 3

let byte_msb b =
  let mask = Uint8.of_int 0b10000000 in
  Uint8.logand b mask

let byte_lsb b =
  let mask = Uint8.of_int 0b00000001 in
  Uint8.logand b mask
