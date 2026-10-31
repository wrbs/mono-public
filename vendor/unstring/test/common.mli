open! Core

val run : 'a Unstring.t -> string -> 'a list
val run_seq : 'a Unstring.t -> string -> 'a Sequence.t
val run_unambiguous : ('a -> Sexp.t) -> 'a Unstring.t -> string -> 'a Or_error.t option
