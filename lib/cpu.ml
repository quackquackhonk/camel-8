open Stdint

type cpu = {
    memory: Memory.t;
    variables: Register.bank;
    pc: uint16;
    index: uint16;
    stack: uint16 list;
    delay_timer: uint8;
    sound_timer: uint8;
  }

let init _ = {
    memory = Memory.init ();
    variables = Register.init ();
    pc = Uint16.zero;
    index = Uint16.zero;
    stack = [];
    delay_timer = Uint8.zero;
    sound_timer = Uint8.zero;
  }
