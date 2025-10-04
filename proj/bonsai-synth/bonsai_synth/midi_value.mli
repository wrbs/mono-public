open! Core

type t = private int [@@deriving sexp, string, enumerate]

include Comparable.S with type t := t
include Hashable.S with type t := t

val of_int : int -> t option
val of_int_exn : int -> t
val to_int : t -> int
