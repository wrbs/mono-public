[@@@disable_unused_warnings]

module Stable = struct
  open Base
  open Bin_prot.Std

  module V1 = struct
    type t = { value : int } [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 51c2ef3016cf616a9fe2bd32453d1f3b |}]
    ;;

    let%expect_test "sexp-digest" =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| ddb261c458fb829f1dd7d134f285de81 |}]
    ;;
  end

  module V1_alternate_sexp = struct
    include V1

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 51c2ef3016cf616a9fe2bd32453d1f3b |}]
    ;;

    (* These sexp functions pass roundtripping, but break serialization when we switch
       between these functions and those in V1 *)
    let sexp_of_t t =
      Sexp.(List [ List [ Atom "value"; Atom (t.value - 1 |> Int.to_string) ] ])
    ;;

    let t_of_sexp sexp =
      let off_by_one = t_of_sexp sexp in
      { value = off_by_one.value + 1 }
    ;;

    let%expect_test "sexp-digest" =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| ddb261c458fb829f1dd7d134f285de81 |}]
    ;;
  end

  module V1_no_roundtrip = struct
    include V1

    let t_of_sexp = V1_alternate_sexp.t_of_sexp

    let%expect_test "fail roundtripping" =
      Expect_test_helpers_base.show_raise (fun () ->
        Stdio.print_endline [%sexp_digest: t]);
      [%expect
        {|
        ddb261c458fb829f1dd7d134f285de81
        "did not raise"
        |}]
    ;;
  end
end
