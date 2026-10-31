open! Base

(* Ptyp_tuples *)
let%expect_test "(float * int)" =
  Stdio.print_endline [%sexp_digest: float * int];
  [%expect {| 5d3a48865f612259939f59e10b41d82f |}]
;;

let%expect_test "(Int.t * (int option))" =
  Stdio.print_endline [%sexp_digest: int * int option];
  [%expect {| 346a5f3663eef2ad3eff82e71de016d2 |}]
;;

let%expect_test "[`A | `B] * string * Stable.V1" =
  Stdio.print_endline [%sexp_digest: [ `A | `B ] * string * Simple_stable.Stable.V1.t];
  [%expect {| 1f8567306082bfb0d95dfacf752b76be |}]
;;

(* Parameterized ptyp_constr *)
let%expect_test "int option" =
  Stdio.print_endline [%sexp_digest: int option];
  [%expect {| 2d76de407ed0652a8d4ba8ba8186ef58 |}]
;;

type ('a, 'b) t =
  { foo : 'a
  ; bar : 'a
  ; baz : 'b
  }
[@@deriving bin_io, sexp, sexp_grammar]

let%expect_test "(int, unit) t" =
  Stdio.print_endline [%sexp_digest: (int, unit) t];
  [%expect {| fa611f9853c8df0a5626412e40380d85 |}]
;;

let%expect_test "(int, [`A | `B] * string * Stable.V1) t" =
  Stdio.print_endline
    [%sexp_digest: (int, [ `A | `B ] * string * Simple_stable.Stable.V1.t) t];
  [%expect {| ea1f465d53178e9017f1ed2dd9cae0dc |}]
;;

(* Ptyp_variants *)
let%expect_test "[`A | `B]" =
  Stdio.print_endline [%sexp_digest: [ `A | `B ]];
  [%expect {| 84144ba87ccdd9a05cbca6bc23c27b09 |}]
;;

(* Record *)
module Big_record = struct
  type t =
    { a1 : int
    ; a2 : int
    ; a3 : int
    ; a4 : int
    ; a5 : int
    ; a6 : int
    ; a7 : int
    ; a8 : int
    ; a9 : int
    ; a10 : int
    ; a11 : int
    ; a12 : int
    ; a13 : int
    ; a14 : int
    ; a15 : int
    ; a16 : int
    ; a17 : int
    ; a18 : int
    ; a19 : int
    ; a20 : int
    }
  [@@deriving sexp, sexp_grammar]
end

let%expect_test "big_record" =
  Stdio.print_endline [%sexp_digest: Big_record.t];
  [%expect {| 75e06d6164cdf4cb51b122495c8c2369 |}]
;;
