open! Core

(** Combinators for building [string Quickcheck.Generator.t] values. These are useful for
    generating strings that match particular parser structures (concatenation, repetition,
    choice, etc.) without coupling to any specific parser representation. *)

type t = string Quickcheck.Generator.t

val empty : t
val literal : string -> t
val of_char : char Quickcheck.Generator.t -> t
val concat : t -> t -> t
val concat_list : t list -> t
val star : t -> t
val repeat : sep:t -> n:int Quickcheck.Generator.t -> t -> t
val choice : t list -> t
