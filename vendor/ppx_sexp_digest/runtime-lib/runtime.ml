open! Base

let hash ~sexp_of_t ~sexp_grammar =
  let string_of_grammar =
    Sexp_grammar.sexp_of_t sexp_of_t sexp_grammar |> Sexp.to_string
  in
  string_of_grammar |> Md5_lib.string |> Md5_lib.to_hex
;;

let sexp_digest ~sexp_of_t ~sexp_grammar = hash ~sexp_of_t ~sexp_grammar
