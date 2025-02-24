open Stdint

type t = {
    cpu: Cpu.t;
    ram: Memory.t;
    display: Display.t;
  }

let create prog = {
    cpu = Cpu.create ();
    ram = Memory.create prog;
    display = Display.create ();
  }

type jump = Next | Skip | Goto of uint16

let handle_jump emu jump =
  match jump with
    Next -> { emu with cpu = Cpu.incr_pc emu.cpu }
  | Skip ->  { emu with cpu = Cpu.incr_pc @@ Cpu.incr_pc emu.cpu }
  | Goto x ->{ emu with cpu = Cpu.set_pc emu.cpu x }

let execute ?key emu inst =
  let open Decode in
  let open Printf in
  match inst with
    MachineRoutine i    -> (emu, Next) (* TODO: Skipping for now... *)
  | Clear               -> ({ emu with display = Display.create ()}, Next)
  | Jump t              -> (emu, Goto t)
  | JumpOffset off      -> raise (Failure "unimplemented")
  | CallSubroutine rout -> raise (Failure "unimplemented")
  | Return              -> raise (Failure "unimplemented")
  | If (x, c)           -> raise (Failure "IF unimplemented")
  | SetN (x, n)         -> let e = { emu with cpu = Cpu.set_register emu.cpu ~reg:x ~data:n} in
                           (e, Next)
  | AddN (x, n)         -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let e = { emu with cpu = Cpu.set_register emu.cpu ~reg:x ~data:Uint8.(xv + n) } in
                           (e, Next)
  | SetXY (x, y)        -> let yv = Cpu.get_register emu.cpu ~reg:y in
                           let e = { emu with cpu = Cpu.set_register emu.cpu ~reg:x ~data:yv } in
                           (e, Next)
  | Binop (op, x, y)    -> let e =  {emu with cpu = Cpu.binop emu.cpu op ~x ~y} in
                           (e, Next)
  | SetIndex (n)        -> let e = {emu with cpu = Cpu.set_index emu.cpu ~data:n} in
                           (e, Next)
  | AddIndex (x)        -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let data = Uint16.(emu.cpu.index + (Uint8.to_uint16 xv)) in
                           let e = {emu with cpu = Cpu.set_index emu.cpu ~data} in
                           (e, Next)
  | Random (x, n)       -> raise (Failure "random")
  | Display (x, y, n)   -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let yv = Cpu.get_register emu.cpu ~reg:y in
                           let s = Memory.read_byte emu.ram emu.cpu.index in
                           let (d, c) = Display.draw emu.display ~x:xv ~y:yv ~s ~n in
                           let e = {emu with cpu = Cpu.set_carry emu.cpu c; display = d} in
                           (e, Next)
  | IfKeyPressed x      -> raise (Failure "key pressed")
  | IfKeyNotPressed x   -> raise (Failure "key not pressed")
  | GetDelayTimer x     -> raise (Failure "get delay")
  | SetDelayTimer x     -> raise (Failure "set delay")
  | SetSoundTimer x     -> raise (Failure "set sound")
  | GetKey x            -> raise (Failure "get key")
  | FontChar x          -> raise (Failure "font")
  | BinToDec x          -> raise (Failure "bin -> dec")
  | WriteMemory x       -> raise (Failure "write")
  | ReadMemory x        -> raise (Failure "read")

let update ?key emu =
  let open Printf in
  (* FETCH instruction *)
  let inst = Memory.read_word emu.ram emu.cpu.pc in
  let _ = printf "PC: %s - Instruction %s\n" (Uint16.to_string_hex emu.cpu.pc) (Uint16.to_string_hex inst) in
  (* decode instruction *)
  let inst = Decode.decode inst in
  (* execute instruction *)
  let (e, j) = execute ?key emu inst in
  handle_jump emu j

let render emu = ()
