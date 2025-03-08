open Stdint

type t = {
    cpu: Cpu.t;
    ram: Memory.t;
    display: Display.t;
  }

type jump = Wait | Next | Skip | Goto of uint16

let create prog = {
    cpu = Cpu.create ();
    ram = Memory.create prog;
    display = Display.create ();
  }

let handle_jump emu jump =
  match jump with
  | Wait -> emu
  | Next -> { emu with cpu = Cpu.incr_pc emu.cpu }
  | Skip ->  { emu with cpu = Cpu.incr_pc @@ Cpu.incr_pc emu.cpu }
  | Goto x ->{ emu with cpu = Cpu.set_pc emu.cpu x }

let check_key key xv eq =
  let convert x = Hex.to_int x |> Uint8.of_int in
  match key with
    None -> Skip
  | Some k when eq (convert k) xv -> Next
  | _ -> Skip

let execute ?(key = None) emu inst =
  let open Decode in
  match inst with
    MachineRoutine i    -> (emu, Next) (* TODO: Skipping for now... *)
  | Clear               -> ({ emu with display = Display.create ()}, Next)
  | Jump t              -> (emu, Goto t)
  | JumpOffset off      -> let v0 = Cpu.get_register emu.cpu ~reg:Hex.Zero in
                           (emu, Goto Uint16.(of_uint8 v0 + off))
  | CallSubroutine addr -> let e = { emu with cpu = Cpu.stack_push emu.cpu addr } in
                           (e, Goto addr)
  | Return              -> let (addr, cpu) = Cpu.stack_pop emu.cpu in
                           let e = { emu with cpu = cpu } in
                           (e, Goto addr)
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
                           let carry = Uint16.(data >= of_int 0x1000) in
                           let emu = { emu with cpu = Cpu.set_index emu.cpu ~data } in
                           let emu = { emu with cpu = Cpu.set_carry emu.cpu carry } in
                           (emu, Next)
  | Random (x, n)       -> let num = Random.full_int 256 |> Uint8.of_int in
                           let num = Uint8.logand num n in
                           let emu = { emu with cpu = Cpu.set_register emu.cpu ~reg:x ~data:num } in
                           (emu, Next)
  | Display (x, y, n)   -> let emu = { emu with cpu = Cpu.set_carry emu.cpu false } in
                           let x = Cpu.get_register emu.cpu ~reg:x in
                           let y = Cpu.get_register emu.cpu ~reg:y in
                           let get_sprite x = Memory.read_byte emu.ram Uint16.(emu.cpu.index + of_int x) in
                           let sprite = List.init n get_sprite in
                           let (d, c) = Display.draw emu.display ~x ~y ~sprite in
                           let e = { emu with cpu = Cpu.set_carry emu.cpu c; display = d } in
                           (e, Next)
  | If (x, c)           -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let cmp = begin match c with
                                     | EqN r    -> fun l -> l = r
                                     | NEqN r   -> fun l -> l <> r
                                     | EqR reg  -> fun l -> l = (Cpu.get_register emu.cpu ~reg)
                                     | NEqR reg -> fun l -> l <> (Cpu.get_register emu.cpu ~reg)
                                     end in
                           let jump = if cmp xv then Skip else Next in
                           (emu, jump)
  | IfKeyPressed x      -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let jump = check_key key xv (=) in
                           (emu, jump)
  | IfKeyNotPressed x   -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let jump = check_key key xv (<>) in
                           (emu, jump)
  | GetKey x            -> raise (Failure "get key")
  | GetDelayTimer x     -> let data = Cpu.get_delay emu.cpu in
                           let emu = { emu with cpu = Cpu.set_register emu.cpu ~reg:x ~data} in
                           (emu, Next)
  | SetDelayTimer x     -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let emu = { emu with cpu = Cpu.set_delay emu.cpu ~data:xv } in
                           (emu, Next)
  | SetSoundTimer x     -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           let emu = { emu with cpu = Cpu.set_sound emu.cpu ~data:xv } in
                           (emu, Next)
  | FontChar x          -> let xv = Cpu.get_register emu.cpu ~reg:x in
                           (* Get the last 4 bits of xv *)
                           let xv = (Uint8.to_int xv) mod 16 in
                           let charp = Memory.font_start + (xv * 2) |> Uint16.of_int in
                           let emu = { emu with cpu = Cpu.set_index emu.cpu ~data:charp } in
                           (emu, Next)
  | BinToDec x          -> let xv = Cpu.get_register emu.cpu ~reg:x |> Uint8.to_int in
                           let hundreds = xv / 100 |> Char.chr in
                           let xv = xv / 100 in
                           let tens = xv / 10 |> Char.chr in
                           let ones = xv mod 10 |> Char.chr in
                           (* *I = hundreds; *(I+1) = tens; *(I+2) = ones *)
                           let i = Cpu.get_index emu.cpu in
                           let _ = Memory.write_byte ~addr:i ~data:hundreds emu.ram in
                           let _ = Memory.write_byte ~addr:Uint16.(i + of_int 1) ~data:tens emu.ram in
                           let _ = Memory.write_byte ~addr:Uint16.(i + of_int 2) ~data:ones emu.ram in
                           (emu, Next)
  (* Write registers v0 to vX to addresses I to I + (X - 1)*)
  | WriteMemory x       -> let num_bytes = Hex.to_int x in
                           let regs = List.init num_bytes Hex.of_int
                                      |> List.map (fun r -> Register.get emu.cpu.regs ~reg:r)
                                      |> List.map Uint8.to_int
                                      |> List.map Char.chr
                           in
                           let addrs = List.init num_bytes (fun idx -> Uint16.(emu.cpu.index + of_int idx)) in
                           let write_mem (data, addr) = Memory.write_byte emu.ram ~data ~addr in
                           List.combine regs addrs |> List.iter write_mem;
                           (emu, Next)
  (* Read byte from addresses I to I + (X - 1) to registers v0 to vX *)
  | ReadMemory x        -> let num_bytes = Hex.to_int x in
                           let regs = List.init num_bytes Hex.of_int in
                           let data = List.init num_bytes (fun idx -> Uint16.(emu.cpu.index + of_int idx))
                                      |> List.map (fun addr -> Memory.read_byte emu.ram addr)
                           in
                           let write_reg cpu (reg, data) = Cpu.set_register cpu ~reg ~data in
                           let cpu = List.combine regs data
                                     |> List.fold_left write_reg emu.cpu in
                           ({ emu with cpu = cpu }, Next)

let update ?key emu =
  (* FETCH instruction *)
  let inst = Memory.read_word emu.ram emu.cpu.pc in
  (* decode instruction *)
  let inst = Decode.decode inst in
  (* execute instruction *)
  let (e, j) = execute ?key emu inst in
  handle_jump e j

let render emu = ()
