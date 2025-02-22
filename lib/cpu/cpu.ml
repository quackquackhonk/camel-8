open Stdint
open Timer

type t = {
    regs: Register.bank;
    pc: uint16;
    index: uint16;
    stack: uint16 list;
    delay_timer: uint8;
    sound_timer: uint8;
  }


let create () = {
    regs = Register.init_bank ();
    pc = Uint16.of_int @@ Memory.instruction_start;
    index = Uint16.zero;
    stack = [];
    delay_timer = Uint8.zero;
    sound_timer = Uint8.zero;
  }

let incr_pc cpu = { cpu with pc = Uint16.(cpu.pc + one) }
let set_pc cpu n = { cpu with pc = n }

let get_register cpu ~reg = Register.get cpu.regs ~reg
let set_register cpu ~reg ~data =
  let bank = Register.set cpu.regs ~reg ~data in
  { cpu with regs = bank }

let get_index cpu       = cpu.index
let set_index cpu ~data = {cpu with index = data }

let get_delay cpu       = cpu.delay_timer
let set_delay cpu ~data = { cpu with delay_timer = data }
let set_sound cpu ~data = { cpu with sound_timer = data }

let binop cpu op ~x ~y =
  let xv = get_register cpu ~reg:x in
  let yv = get_register cpu ~reg:y in
  let res =
    let open Decode in
    match op with
      Add -> Uint8.(xv + yv)
    | Sub -> Uint8.(xv - yv)
    | OR -> Uint8.logand xv yv
    | AND -> Uint8.logor xv yv
    | XOR -> Uint8.logxor xv yv
    | ShiftLeft -> Uint8.shift_left yv 1
    | ShiftRight -> Uint8.shift_right_logical yv 1
  in
  let carry =
    let open Decode in
    match op with
      Add -> res < xv
    | Sub -> xv > yv
    | OR | AND | XOR -> false
    | ShiftLeft -> Uint8.zero <> Binary.byte_msb yv
    | ShiftRight -> Uint8.zero <> Binary.byte_lsb yv
  in
  { cpu with regs = Register.set_carry cpu.regs carry }
  |> set_register ~reg:x ~data:res
