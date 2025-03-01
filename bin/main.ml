open Camel8

let usage_msg = "camel8 [--debug] [--tui] <rom>"
let debug = ref false
let tui = ref false
let rom = ref ""

let anon_fun fname = rom := fname

let speclist =
  [("--debug", Arg.Set debug, "Output debug information");
   ("--tui", Arg.Set tui, "Run in terminal mode")]

let () =
  Arg.parse speclist anon_fun usage_msg;
  try let prog_bytes = Io.get_bytes !rom in
      let emu = Emu.create prog_bytes in
      Tui.run ~debug:!debug emu
  with Decode.Decode_error msg ->
        Printf.printf "Error decoding an instruction:\n\t%s\n" msg
     | Sys_error msg ->
        Printf.printf "Unexpected error:\n\t%s\n" msg
