open Stdint
open Timer

type t = {
    variables: Register.bank;
    pc: uint16;
    index: uint16;
    stack: uint16 list;
    delay_timer: uint8;
    sound_timer: uint8;
  }


let create () = {
    variables = Register.init_bank ();
    pc = Uint16.of_int @@ Memory.instruction_start;
    index = Uint16.zero;
    stack = [];
    delay_timer = Uint8.zero;
    sound_timer = Uint8.zero;
  }
