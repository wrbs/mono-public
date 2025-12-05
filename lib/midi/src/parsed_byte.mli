@@ portable

open! Core

type t =
  | Value of Value.t
  | Status of Status.t
[@@deriving sexp_of, compare ~localize, equal ~localize, enumerate]

val to_byte : t -> Byte.t
val of_byte : Byte.t -> t
