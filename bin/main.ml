open Camel8

let usage_msg = "camel8 [--debug] [--tui] <rom>"
let debug = ref false
let tui = ref false
let rom = ref ""

let anon_fun fname = rom := fname

let speclist =
  [("--debug", Arg.Set debug, "Output debug information");
   ("--tui", Arg.Set tui, "Run in terminal mode")]

let main _ =
  let prog_bytes = Bytes.make 0 (Char.chr 0) in
  let emu = Emu.create prog_bytes in
  Emu.run emu


let () =
  Arg.parse speclist anon_fun usage_msg;
  try main !rom
  with Decode.Decode_error msg ->
        Printf.printf "Error decoding an instruction:\n\t%s\n" msg
     | Sys_error msg ->
        Printf.printf "Unexpected error:\n\t%s\n" msg
