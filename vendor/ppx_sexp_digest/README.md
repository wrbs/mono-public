ppx\_sexp\_digest
=================

# Overview

A ppx extension for hashing the sexp_grammar of a type. This is meant to be used similarly
to `[%bin_digest: t]`. Common sexp serialization changes like adding record field
annotations don't affect the `[%bin_digest: t]` output but do affect this ppx output.

# Example Usage
```ocaml
module V1 = struct
  type t = <...> [@@deriving sexp_grammar, sexp]

  let%expect_test _ =
    print_endline [%sexp_digest: t];
    [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
  ;;
end
```
