open! Core

let run parser str =
  let compiled = Unstring.compile parser in
  Unstring.Compiled.run_all_parses compiled str
  |> Sequence.map ~f:Or_error.ok_exn
  |> Sequence.to_list
;;

let run_seq parser str =
  let compiled = Unstring.compile parser in
  Unstring.Compiled.run_all_parses compiled str |> Sequence.map ~f:Or_error.ok_exn
;;

let run_unambiguous sexp_of parser str =
  let compiled = Unstring.compile parser in
  Unstring.Compiled.run_unambiguous sexp_of compiled str
;;
