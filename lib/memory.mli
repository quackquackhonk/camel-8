open Stdint

type t
type address = uint16
type error

exception Memory_error of string

val memory_size : int
val instruction_start : int

val init : bytes -> t

val read : t -> address -> char
val write : t -> address -> char -> unit
