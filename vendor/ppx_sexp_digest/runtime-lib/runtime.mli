open! Base

(** Non-code-writing functions used by [ppx_sexp_digest.ml] *)

val sexp_digest : sexp_of_t:('a -> Sexp.t) -> sexp_grammar:'a Sexp_grammar.t -> string
