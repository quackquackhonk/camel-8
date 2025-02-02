open Camel8

let usage_msg = "camel8 [--debug] [--tui] <rom>"
let debug = ref false
let tui = ref false
let rom = ref ""

let anon_fun fname = rom := fname

let speclist =
  [("--debug", Arg.Set debug, "Output debug information");
   ("--tui", Arg.Set tui, "Run in terminal mode")]

let main ch =
  let rom_types = input_byte in
  let cpu = Cpu.init_cpu () in
  ()

let () =
  Arg.parse speclist anon_fun usage_msg;
  let fname = !rom in
  try
    let ch = open_in_bin fname in
    main ch
  with Sys_error msg ->
    Printf.printf "Error when opening ROM %s:\n\t%s\n" fname msg
