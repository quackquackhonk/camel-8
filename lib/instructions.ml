type opcode = MachineRoutine
            | Clear
            | Jump
            | JumpOffset
            | CallSubroutine
            | Return
            | SkipEqualN
            | SkipNotEqualN
            | SkipEqual
            | SkipNotEqual
            | SetN
            | AddN
            | SetToY
            | OR
            | AND
            | XOR
            | Add
            | Sub
            | Shift
            | SetIndex
            | AddIdex
            | Random
            | Display
            | SkipKeyPressed
            | SkipKeyNotPressed
            | GetDelayTimer
            | SetDelayTimer
            | SetSoundTimer
            | GetKey
            | FontChar
            | BinToDec
            | WriteMemory
            | ReadMemory

type t = {
    op: opcode;
    x: int;
    y: int;
    n: int;
  }

let create code x y n = {
    op=code; x=x; y=y; n=n
  }


let parse _ = create MachineRoutine 0 0 0
