type t = {
    mutable cpu: Cpu.t;
    mutable ram: Memory.t;
    display: Display.t;
    mutable timer: Timer.t;
  }

let create prog = {
    cpu = Cpu.create ();
    ram = Memory.create prog;
    display = Display.create ();
    timer = Timer.create 60.0;
  }

let execute emu inst =
  let open Decode in
  let open Printf in
  match inst with
    Clear -> printf "got clear"
  | _ -> printf "didn't get clear"

let rec update ~cycles emu =
  if cycles <= 0
  then ()
  else
    let cycles = cycles - 1 in
    (* fetch instruction *)
    let inst = Memory.read_word emu.ram emu.cpu.pc in
    (* decode instruction *)
    let inst = Decode.decode inst in
    (* execute instruction *)
    execute emu inst;
    (* TODO: update display *)
    update ~cycles emu


let run emu  =
  while true do
    let cycles = Timer.advance emu.timer in begin
        update ~cycles emu;
        emu.timer <- Timer.update (emu.timer)
      end
  done
