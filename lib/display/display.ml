open Stdint

type t = uint64 array

let leftmost_bit = Uint64.of_string "0x8000000000000000"

let create () = Array.make 32 Uint64.zero

let draw_sprite d ~x ~y ~s =
  let row = Array.get d y in
  (* Function for shifting the sprite into position
     Initial sprite:
     0                             55     63
     v                             v      v
     [                             ssssssss]

     If x is > 55, shift right, otherwise shift left
   *)
  let adjust sprite =
    let thresh = 63 - 8 in
    if x > thresh
    then Uint64.shift_right_logical sprite (x - thresh)
    else Uint64.shift_left sprite (thresh - x)
  in
  let sprite = adjust @@ Uint64.of_uint8 s in
  let mask = adjust @@ Uint64.of_uint8 (Uint8.max_int) in
  let new_row = Uint64.logxor row sprite in
  Array.set d y new_row;
  (d, Uint64.((logand mask row) = zero))

let draw d ~x ~y ~sprite =
  let carry = ref false in
  let display = ref d in
  let x = Uint8.to_int x mod 64 in
  let y = Uint8.to_int y mod 32 in
  List.iteri (fun offset s ->
    let yn = y + offset in
    let (d, c) = draw_sprite !display ~x ~y:yn ~s in
    carry := !carry || c;
    display := d) sprite;
  (!display, !carry)

let to_string d =
  let get_pixel row c =
    let mask = Uint64.shift_right_logical leftmost_bit c in
    let has_pixel = Uint64.logand row mask <> Uint64.zero in
    if has_pixel then 'X' else '.'
  in
  let draw_row row = Seq.init 64 (get_pixel row) |> String.of_seq in
  let strs = Array.map draw_row d in
  Array.fold_left (fun a b -> a ^ "\n" ^ b) "" strs
