open Stdint
open Timer

type t = {
    memory: Memory.t;
    variables: Register.bank;
    pc: uint16;
    index: uint16;
    stack: uint16 list;
    delay_timer: uint8;
    sound_timer: uint8;
  }

let init_cpu bytes = {
    memory = Memory.init bytes;
    variables = Register.init_bank ();
    pc = Uint16.of_int @@ Memory.instruction_start;
    index = Uint16.zero;
    stack = [];
    delay_timer = Uint8.zero;
    sound_timer = Uint8.zero;
  }

let fetch cpu = Memory.read cpu.memory cpu.pc

let execute cpu inst =
  let open Decode in
  let open Printf in
  match inst with
    Clear -> printf "got clear"
  | _ -> printf "didn't get clear"
