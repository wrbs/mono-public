open! Core

(* Ptyp_tuples *)
let%expect_test "(float * int)" =
  print_endline [%bin_and_sexp_digest: float * int];
  [%expect {| 30786ceab3f284818b8930fe0a6314e5 |}]
;;

let%expect_test "(Int.t * (int option))" =
  print_endline [%bin_and_sexp_digest: Int.t * int option];
  [%expect {| b2d57b156b2658d93ed317ba7711fcb6 |}]
;;

let%expect_test "[`A | `B] * string * Stable.V1" =
  print_endline [%bin_and_sexp_digest: [ `A | `B ] * string];
  [%expect {| 56898179500c691559d6c3b588834db9 |}]
;;

(* Parameterized ptyp_constr *)
let%expect_test "int option" =
  print_endline [%bin_and_sexp_digest: int option];
  [%expect {| dadd81aae3267d743db7c46f98eea9bd |}]
;;

type ('a, 'b) t =
  { foo : int
  ; bar : 'a
  ; baz : 'b
  }
[@@deriving bin_io, sexp_grammar, sexp]

let%expect_test "(int, unit) t" =
  print_endline [%bin_and_sexp_digest: (int, unit) t];
  [%expect {| ba156d985d7bb1aadd170f8438b88906 |}]
;;

let%expect_test "(int, [`A | `B] * string * Stable.V1) t" =
  print_endline [%bin_and_sexp_digest: (int, [ `A | `B ] * string) t];
  [%expect {| 2aff2ec44ad1517afa89c794dea85b77 |}]
;;

(* Ptyp_variants *)
let%expect_test "[`A | `B]" =
  print_endline [%bin_and_sexp_digest: [ `A | `B ]];
  [%expect {| 818fe13658c243b235d06159c9d2ee78 |}]
;;
