@@ portable

open! Core

type t =
  | C1
  | C2
  | C3
  | C4
  | C5
  | C6
  | C7
  | C8
  | C9
  | C10
  | C11
  | C12
  | C13
  | C14
  | C15
  | C16
[@@deriving quickcheck, enumerate]

include Identifiable.S [@mode local] with type t := t

val to_int : t -> int
val of_int : int -> t option
val of_int_exn : int -> t
val of_lower_bits : Byte.t -> t
val to_lower_bits : t -> Byte.t
val next_wrap : t -> t
val prev_wrap : t -> t
val arg_type : t Command.Arg_type.t
