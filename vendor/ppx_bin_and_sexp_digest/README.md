ppx\_bin\_and\_sexp\_digest
===========================

# Overview

A ppx extension for hashing the sexp_grammar and bin_prot shape of a type. This is meant
to be used in-place of `[%bin_digest: t]`. See `ppx_sexp_digest` and `ppx_bin_prot` for
more details about each ppx.

# Example Usage
```ocaml
module V1 = struct
  type t = <...> [@@deriving bin_io, sexp_grammar, sexp]

  let%expect_test _ =
    print_endline [%bin_and_sexp_digest: t];
    [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
  ;;
end
```
