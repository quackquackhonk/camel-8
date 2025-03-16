open Stdint

type t = bytes

exception Memory_error of string

let memory_size = 4096
let font_start = 0x050
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

let pretty ?(offset = 0) mem pc =
  let pci = Uint16.to_int pc in
  let (s, num) = if offset <= 0
               then (0, Bytes.length mem / 2)
               else (pci - (offset * 2), offset * 2 + 1)
  in
  (* Create list of addresses *)
  let addrs = List.init num (fun a -> s + (a * 2)) in
  let make_line addr =
    try let addr = Uint16.of_int addr in
        let pref = if addr = pc then ">" else " " in
        let x = read_word mem addr in
        Printf.sprintf "%s %s: %s" pref (Pretty.uint16_to_hex_string addr) (Pretty.uint16_to_hex_string x)
    with Invalid_argument msg -> ""
  in
  List.map make_line addrs
  |> List.filter (fun s -> s <> "")

let create prog =
  let progl = Bytes.length prog in
  let _ = valid_program progl in
  let mem = Bytes.init memory_size (fun _ -> Char.chr 0) in
  (* Load the font into memory *)
  let fontl = Bytes.length Font.font in
  let _= Bytes.blit Font.font 0 mem font_start fontl in
  (* Load the program *)
  let _ = Bytes.blit prog 0 mem instruction_start progl in
  mem
