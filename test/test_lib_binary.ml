open Camel8
open Stdint

let case = Uint16.of_int 0x1234

(* The tests *)
let test_nibbles () =
  Alcotest.(check int) "first nibble is 1" 1 (Uint16.to_int @@ Binary.first_nibble case);
  Alcotest.(check int) "second nibble is 2" 2 (Uint16.to_int @@ Binary.second_nibble case);
  Alcotest.(check int) "third nibble is 3" 3 (Uint16.to_int @@ Binary.third_nibble case);
  Alcotest.(check int) "fourth nibble is 4" 4 (Uint16.to_int @@ Binary.fourth_nibble case)


(* Run it *)
let () =
  let open Alcotest in
  run "Binary" [
      "Nibbles of 0x0001001000110100", [
        test_case "Simple"     `Quick test_nibbles;
      ];
    ]
