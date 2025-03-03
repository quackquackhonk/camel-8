open Stdint

type t = {
    cpu: Cpu.t;
    ram: Memory.t;
    display: Display.t;
  }

type jump = Next | Skip | Goto of uint16

let create prog = {
    cpu = Cpu.create ();
    ram = Memory.create prog;
    display = Display.create ();
  }

let handle_jump emu jump =
  match jump with
    Next -> { emu with cpu = Cpu.incr_pc emu.cpu }
  | Skip ->  { emu with cpu = Cpu.incr_pc @@ Cpu.incr_pc emu.cpu }
  | Goto x ->{ emu with cpu = Cpu.set_pc emu.cpu x }

let execute ?key emu inst =
  let open Decode in
  match inst with
    MachineRoutine i    -> (emu, Next) (* TODO: Skipping for now... *)
  | Clear               -> ({ emu with display = Display.create ()}, Next)
  | Jump t              -> (emu, Goto t)
  | JumpOffset off      -> raise (Failure "unimplemented")
  | CallSubroutine addr -> let e = { emu with cpu = Cpu.stack_push emu.cpu addr } in
                           (e, Goto addr)
  | Return              -> let (addr, cpu) = Cpu.stack_pop emu.cpu in
                           let e = { emu with cpu = cpu } in
                           (e, Goto addr)
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
  | Display (x, y, n)   -> let emu = { emu with cpu = Cpu.set_carry emu.cpu false } in
                           let x = Cpu.get_register emu.cpu ~reg:x in
                           let y = Cpu.get_register emu.cpu ~reg:y in
                           let get_sprite x = Memory.read_byte emu.ram Uint16.(emu.cpu.index + of_int x) in
                           let sprite = List.init n get_sprite in
                           let (d, c) = Display.draw emu.display ~x ~y ~sprite in
                           let e = { emu with cpu = Cpu.set_carry emu.cpu c; display = d } in
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
  (* FETCH instruction *)
  let inst = Memory.read_word emu.ram emu.cpu.pc in
  (* decode instruction *)
  let inst = Decode.decode inst in
  (* execute instruction *)
  let (e, j) = execute ?key emu inst in
  handle_jump e j

let render emu = ()
