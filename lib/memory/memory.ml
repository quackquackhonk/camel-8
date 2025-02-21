open Stdint

type t = bytes

exception Memory_error of string

let memory_size = 4096
let instruction_start = 0x200

let valid_program size =
  let max_prog_size = memory_size - instruction_start in
  if size >= max_prog_size
  then let msg = Printf.sprintf "program size %d > memory size %d" size max_prog_size in
       raise (Memory_error msg)

let create prog =
  let progl = Bytes.length prog in
  let _ = valid_program progl in
  let mem = Bytes.init memory_size (fun _ -> Char.chr 0) in
  let _ = Bytes.blit prog 0 mem instruction_start progl in
  mem

(*TODO: unimplemented*)
let read_byte mem addr = Uint8.zero
let write_byte mem ~addr ~data = ()

let read_word mem addr = Uint16.zero
let write_word mem ~addr ~data = ()
