open Stdint

(** Interface that provide 8-bit read/write *)
module type ByteS = sig
  type t
  (* reads 8-bit data from address addr *)
  val read_byte : t -> uint16 -> uint8
  (* writes 8-bit data to address addr *)
  val write_byte : t -> addr:uint16 -> data:uint8 -> unit
end

(** Interface that provide 16-bit read/write *)
module type WordS = sig
  type t
  (* 16-bit reads/writes *)
  val read_word : t -> uint16 -> uint16
  val write_word : t -> addr:uint16 -> data:uint16 -> unit
end
