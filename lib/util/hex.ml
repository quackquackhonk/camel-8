open Stdint

type t = Zero
         | One
         | Two
         | Three
         | Four
         | Five
         | Six
         | Seven
         | Eight
         | Nine
         | A
         | B
         | C
         | D
         | E
         | F

let to_int = function
    Zero -> 0
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Five -> 5
  | Six -> 6
  | Seven -> 7
  | Eight -> 8
  | Nine -> 9
  | A -> 10
  | B -> 11
  | C -> 12
  | D -> 13
  | E -> 14
  | F -> 15

let to_string = function
    Zero -> "0"
  | One -> "1"
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"

let of_int = function
    0 -> Zero
  | 1 -> One
  | 2 -> Two
  | 3 -> Three
  | 4 -> Four
  | 5 -> Five
  | 6 -> Six
  | 7 -> Seven
  | 8 -> Eight
  | 9 -> Nine
  | 10 -> A
  | 11 -> B
  | 12 -> C
  | 13 -> D
  | 14 -> E
  | 15 -> F
  | x -> raise (Failure ("Invalid Hex digit: " ^ (string_of_int x)))


let uint16_to_hex_string n =
  let open Binary in
  let convert f n = f n |> Uint16.to_int |> of_int |> to_string in
  Printf.sprintf "0x%s%s%s%s"
    (convert first_nibble n)
    (convert second_nibble n)
    (convert third_nibble n)
    (convert fourth_nibble n)


let compare l r =
  let li = to_int l in
  let ri = to_int r in
  li - ri
