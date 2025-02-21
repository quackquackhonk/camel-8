open Stdint

type t = uint32 array

let create () = Array.make 64 Uint32.zero
