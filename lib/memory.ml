open Stdint

type t = bytes
type address = uint16

type error = ProgramTooLarge of int

exception Memory_error of string

let memory_size = 4096
let instruction_start = 0x200

let valid_program size =
  let max_prog_size = memory_size - instruction_start in
  if size >= max_prog_size
  then let msg = Printf.sprintf "program size %d > memory size %d" size max_prog_size in
       raise (Memory_error msg)

let init prog =
  let _ = valid_program (Bytes.length prog) in
  let mem = Bytes.init memory_size (fun _ -> Char.chr 0) in
  mem


let read mem addr = Char.chr 0

let write mem addr data = ()
