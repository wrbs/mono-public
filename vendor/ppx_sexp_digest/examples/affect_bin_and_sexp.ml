[@@@disable_unused_warnings]

module Stable = struct
  open Base
  open Bin_prot.Std

  module Original = struct
    type t =
      { description : string option
      ; value : int
      }
    [@@deriving bin_io, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 017a13080c6b4938d4a67866f87e1f33 |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| b8fdc8c035a16b1b9b1719dafa48c9dc |}]
    ;;
  end

  (** Both bin_digest and sexp_digest change *)
  module Switch_fields = struct
    type t =
      { value : int
      ; description : string option
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 9162d850a346c6fd3755545ef68bfb33 |}]
    ;;
  end

  module Int_alias = struct
    type t = int [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]
  end

  (** Neither bin_digest nor sexp_digest change *)
  module Type_alias = struct
    type t =
      { value : Int_alias.t
      ; description : string option
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 9162d850a346c6fd3755545ef68bfb33 |}]
    ;;
  end

  (** Both bin_digest and sexp_digest change *)
  module Rename_fields = struct
    type t =
      { number : int
      ; description : string option
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 4a6bf0f0d82c2767e5c16192128467c5 |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 821debd09051e78abc86020981314f59 |}]
    ;;
  end
end
