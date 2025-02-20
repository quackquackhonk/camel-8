open Cpu
open Memory
open Timer

type t = {
    cpu: Cpu.t;
    ram: Memory.t;
    timer: Timer.t;
    display: Display.t;
  }
