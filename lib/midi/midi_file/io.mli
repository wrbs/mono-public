@@ portable

open! Core

module Read : sig
  type 'a t' = next:(unit -> char option) -> 'a
  type 'a t = 'a Core.Or_error.t t'

  val with_offset_in_error : 'a t -> 'a t
  val subchunk : (at_end:(unit -> bool) -> 'a t') -> len:int -> 'a t'
  val byte : char t
  val string : len:int -> string t
  val u8 : int t
  val i8 : int t
  val u16 : int t
  val u32 : int t
  val variable_length : Num.U28.t t
  val tempo_3_byte : Num.U24.t t
  val skip : int -> unit t
  val value : Midi.Value.t t
end

val read_string : string -> 'a Read.t' -> 'a
