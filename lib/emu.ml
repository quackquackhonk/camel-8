open Stdint

type t = {
    mutable cpu: Cpu.t;
    mutable ram: Memory.t;
    mutable display: Display.t;
    mutable timer: Timer.t;
  }

let create prog = {
    cpu = Cpu.create ();
    ram = Memory.create prog;
    display = Display.create ();
    timer = Timer.create 60.0;
  }

type jump = Next | Skip | Goto of uint16

let handle_jump emu jump =
  match jump with
    Next -> emu.cpu <- Cpu.incr_pc emu.cpu
  | Skip ->  emu.cpu <- emu.cpu |> Cpu.incr_pc |> Cpu.incr_pc
  | Goto x ->  emu.cpu <- Cpu.set_pc emu.cpu x

let execute emu inst =
  let open Decode in
  let open Printf in
  match inst with
    MachineRoutine i -> Next (* TODO: Skipping for now... *)
  | Clear -> emu.display <- Display.create ();
             Next
  | Jump t -> Goto t
  | JumpOffset off -> raise (Failure "unimplemented")
  | CallSubroutine rout -> raise (Failure "unimplemented")
  | Return -> raise (Failure "unimplemented")
  | If (x, c) -> raise (Failure "IF unimplemented")
  | SetN (x, n) -> emu.cpu <- Cpu.set_register emu.cpu ~reg:x ~data:n;
                   Next
  | AddN (x, n) -> let xv = Cpu.get_register emu.cpu ~reg:x in
                   emu.cpu <- Cpu.set_register emu.cpu ~reg:x ~data:Uint8.(xv + n);
                   Next
  | SetXY (x, y) -> let yv = Cpu.get_register emu.cpu ~reg:y in
                    emu.cpu <- Cpu.set_register emu.cpu ~reg:x ~data:yv;
                    Next
  | Binop (op, x, y) -> emu.cpu <- Cpu.binop emu.cpu op ~x ~y;
                        Next
  | SetIndex (n) -> emu.cpu <- Cpu.set_index emu.cpu ~data:n;
                    Next
  | AddIndex (x) -> let xv = Cpu.get_register emu.cpu ~reg:x in
                    let data = Uint16.(emu.cpu.index + (Uint8.to_uint16 xv)) in
                    emu.cpu <- Cpu.set_index emu.cpu ~data;
                    Next
  | Random (x, n) -> raise (Failure "random")
  | Display (x, y, n) -> raise (Failure "display")
  | IfKeyPressed x -> raise (Failure "key pressed")
  | IfKeyNotPressed x -> raise (Failure "key not pressed")
  | GetDelayTimer x -> raise (Failure "get delay")
  | SetDelayTimer x -> raise (Failure "set delay")
  | SetSoundTimer x -> raise (Failure "set sound")
  | GetKey x -> raise (Failure "get key")
  | FontChar x -> raise (Failure "font")
  | BinToDec x -> raise (Failure "bin -> dec")
  | WriteMemory x -> raise (Failure "write")
  | ReadMemory x -> raise (Failure "read")

let do_cycle emu =
  (* fetch instruction *)
  let inst = Memory.read_word emu.ram emu.cpu.pc in
  (* decode instruction *)
  let inst = Decode.decode inst in
  (* execute instruction *)
  execute emu inst
  |> handle_jump emu

let rec update emu ~cycles =
  if cycles > 0 then begin
      do_cycle emu;
      update ~cycles:(cycles - 1) emu
    end

let render _ = ()

let run emu  =
  while true do
    let cycles = Timer.advance emu.timer in begin
        update ~cycles emu;
        (* TODO: update display *)
        render emu;
        emu.timer <- Timer.update (emu.timer)
      end
  done
