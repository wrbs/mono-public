@@ portable

open! Core

type t = private int [@@deriving string, sexp, equal ~localize, compare ~localize, bin_io]

val create' : r:int -> g:int -> b:int -> t
val create : int -> int -> int -> t
val of_int_truncate : int -> t
val of_string_opt : string -> t option
val r : t -> int
val g : t -> int
val b : t -> int
val parts : t -> r:int * g:int * b:int
