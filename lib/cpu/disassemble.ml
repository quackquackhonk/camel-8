open Stdint

type error =
  IO of string
| EOF of int
[@@deriving show]

exception Disassemble_error of string

let fmt_err e =
  match e with
    IO msg -> Printf.sprintf "IO error: %s" msg
  | EOF pos -> Printf.sprintf "Unexpected EOF at pos %d" pos

let unwrap r =
  match r with
    Ok x -> x
  | Error e -> raise (Disassemble_error (fmt_err e))


let bytes_to_uint16 = if Sys.big_endian
                      then Uint16.of_bytes_big_endian
                      else Uint16.of_bytes_little_endian

let disassemble_inst bytes idx =
  let offset = idx * 2 in
  try Ok (bytes_to_uint16 bytes offset)
  with Invalid_argument _ -> Error (EOF (offset))

let get_bytes fname =
  try
    let s = In_channel.with_open_bin fname In_channel.input_all in
    Ok (Bytes.of_string s)
  with Sys_error msg -> Error (IO msg)

let disassemble fname =
  let bytes = unwrap (get_bytes fname) in
  let len = Bytes.length bytes / 2 in (* Each instruction is 2 bytes *)
  let get_inst idx = disassemble_inst bytes idx |> unwrap in
  Array.init len get_inst
