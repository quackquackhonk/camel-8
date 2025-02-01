open Stdint

let nibbles = [|
    Uint16.of_int 0x1111000000000000;
    Uint16.of_int 0x0000111100000000;
    Uint16.of_int 0x0000000011110000;
    Uint16.of_int 0x0000000000001111;
  |]

let get_nibble nib bin =
  let unshifted = Uint16.logand bin nibbles.(nib) in
  let shift = Uint16.bits - (4 * (nib + 1)) in
  Uint16.shift_right_logical unshifted shift

let first_nibble  = get_nibble 0
let second_nibble = get_nibble 1
let third_nibble  = get_nibble 2
let fourth_nibble = get_nibble 3

let to_NNN bin =
  let mask = Uint16.of_int 0x0000111111111111 in
  Uint16.logand mask bin
