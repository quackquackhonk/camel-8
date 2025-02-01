(* A module with functions to test *)
open Camel8
open Stdint

let case = Uint16.of_int 0x0001001000110100

(* The tests *)
let test_first_nibble () =
  Alcotest.(check int) "first nibble is 1" 1 (Uint16.to_int @@ Binary.first_nibble case)

(* let test_capitalize () = *)
(*   Alcotest.(check string) "same string" "World." (To_test.capitalize "world.") *)

(* let test_str_concat () = *)
(*   Alcotest.(check string) "same string" "foobar" (To_test.str_concat ["foo"; "bar"]) *)

(* let test_list_concat () = *)
(*   Alcotest.(check (list int)) "same lists" [1; 2; 3] (To_test.list_concat [1] [2; 3]) *)


(* Run it *)
let () =
  let open Alcotest in
  run "Binary" [
      "Nibbles of 0x0001001000110100", [
        test_case "First nibble"     `Quick test_first_nibble;
      ];
    ]
