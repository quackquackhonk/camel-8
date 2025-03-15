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

let incr_pc cpu = { cpu with pc = Uint16.(cpu.pc + one + one) }
let set_pc cpu n = { cpu with pc = n }

let get_register cpu ~reg = Register.get cpu.regs ~reg
let set_register cpu ~reg ~data =
  let bank = Register.set cpu.regs ~reg ~data in
  { cpu with regs = bank }
let set_carry cpu carry =
  { cpu with regs = Register.set_carry cpu.regs carry }

let get_index cpu       = cpu.index
let set_index cpu ~data = {cpu with index = data }

let get_delay cpu       = cpu.delay_timer
let set_delay cpu ~data = { cpu with delay_timer = data }
let set_sound cpu ~data = { cpu with sound_timer = data }

let stack_push cpu  = { cpu with stack = cpu.pc :: cpu.stack }
let stack_pop cpu =
  print_endline "stack pop with stack: ";
  List.iter (fun a -> Printf.printf "%s\n" (Uint16.to_string_hex a)) cpu.stack;
  let addr, stack = (List.hd cpu.stack, List.tl cpu.stack) in
  (addr, { cpu with stack = stack })

let pretty_stack ?(max_addrs = Int.max_int) cpu =
  let rec pretty_stack' acc = function
    | [] -> acc
    | x :: xs -> if List.length acc >= max_addrs - 1
                 then "[....]" :: acc
                 else pretty_stack' (Pretty.uint16_to_hex_string x :: acc) xs
  in pretty_stack' [] cpu.stack

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
  set_carry cpu carry
  |> set_register ~reg:x ~data:res
