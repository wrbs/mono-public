[@@@disable_unused_warnings]

module Stable = struct
  open! Core.Core_stable
  open Base_quickcheck

  module V1 = struct
    type t = { value : int } [@@deriving bin_io, equal, sexp_grammar, sexp]

    let%expect_test "bin_and_sexp_digest" =
      print_endline [%bin_and_sexp_digest: t];
      [%expect {| c3694c1dc3fdcc8132624e7f798d2ff2 |}]
    ;;
  end

  module V1_alternate_sexp = struct
    include V1

    (* These sexp functions pass roundtripping, but break serialization when we switch
       between these functions and those in V1 *)
    let sexp_of_t t =
      Core.Sexp.(List [ List [ Atom "value"; Atom (t.value - 1 |> Core.Int.to_string) ] ])
    ;;

    let t_of_sexp sexp =
      let off_by_one = t_of_sexp sexp in
      { value = off_by_one.value + 1 }
    ;;

    let%expect_test "bin_and_sexp_digest" =
      print_endline [%bin_and_sexp_digest: t];
      [%expect {| c3694c1dc3fdcc8132624e7f798d2ff2 |}]
    ;;
  end

  module V1_no_roundtrip = struct
    include V1

    let t_of_sexp = V1_alternate_sexp.t_of_sexp

    let%expect_test "fail roundtripping" =
      Expect_test_helpers_core.show_raise (fun () ->
        print_endline [%bin_and_sexp_digest: t]);
      [%expect
        {|
        c3694c1dc3fdcc8132624e7f798d2ff2
        "did not raise"
        |}]
    ;;
  end
end
