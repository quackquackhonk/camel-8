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


let read_byte mem addr = Bytes.get mem (Uint16.to_int addr) |> Char.code |> Uint8.of_int
let write_byte mem ~addr ~data = Bytes.set mem (Uint16.to_int addr) data

let read_word mem addr =
  let addr = Uint16.to_int addr in
  let fst_byte = Bytes.get mem addr |> Char.code in
  let snd_byte = Bytes.get mem (addr + 1) |> Char.code in
  Int.(logor (shift_left fst_byte 8) snd_byte)
  |> Uint16.of_int



let write_word mem ~addr ~data =
  let set = if Sys.big_endian
            then Uint16.to_bytes_big_endian
            else Uint16.to_bytes_little_endian
  in
  set data mem (Uint16.to_int addr)

let dump ?window mem =
  let (s, e) = match window with
    | None -> (0, Bytes.length mem / 2)
    | Some (w_s, w_e) -> (w_s, w_e)
  in
  let diff = e - s in
  for o = 0 to diff do
    let off = Uint16.of_int (s + (o * 2)) in
    let x = read_word mem off in
    print_endline @@ Uint16.to_string_hex x
  done

let create prog =
  let progl = Bytes.length prog in
  let _ = valid_program progl in
  let mem = Bytes.init memory_size (fun _ -> Char.chr 0) in
  let _ = Bytes.blit prog 0 mem instruction_start progl in
  dump ~window:(0x200, 0x20F) mem;
  mem
