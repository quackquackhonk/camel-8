open Stdint

type address = int
type bank = uint16 array

let num_registers = 16
let v0 = 0
let v1 = 1
let v2 = 2
let v3 = 3
let v4 = 4
let v5 = 5
let v6 = 6
let v7 = 7
let v8 = 8
let v9 = 9
let vA = 10
let vB = 11
let vC = 12
let vD = 13
let vE = 14
let vF = 15

let init _ = Array.make num_registers Uint16.zero
