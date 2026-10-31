[@@@disable_unused_warnings]

module Option_example = struct
  open Base
  open Bin_prot.Std

  module V1 = struct
    type t =
      { value : int
      ; description : string option
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]
  end

  module V2 = struct
    type t =
      { value : int
      ; description : string option [@sexp.option]
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]
  end

  (* V1 *)
  let%expect_test _ =
    Stdio.print_endline [%bin_digest: V1.t];
    [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
  ;;

  let%expect_test _ =
    Stdio.print_endline [%sexp_digest: V1.t];
    [%expect {| 9162d850a346c6fd3755545ef68bfb33 |}]
  ;;

  (* V2 *)
  let%expect_test _ =
    Stdio.print_endline [%bin_digest: V2.t];
    [%expect {| 7832901500807034d4e1d7198b5a0dea |}]
  ;;

  (* When we add [@sexp.option], only the [sexp] changes. *)
  let%expect_test _ =
    Stdio.print_endline [%sexp_digest: V2.t];
    [%expect {| b2244da2e25db288c68c6eb14d8d9521 |}]
  ;;

  (* When we add [@sexp.option], we expect the [bin_digest] to stay the same and the
     [sexp_digest] to differ. *)
  let%expect_test _ =
    let v1_bin_digest = [%bin_digest: V1.t] in
    let v2_bin_digest = [%bin_digest: V2.t] in
    Stdio.printf "%b, " (String.equal v1_bin_digest v2_bin_digest);
    let v1_sexp_digest = [%sexp_digest: V1.t] in
    let v2_sexp_digest = [%sexp_digest: V2.t] in
    Stdio.printf "%b" (String.equal v1_sexp_digest v2_sexp_digest);
    [%expect {| true, false |}]
  ;;
end

module List_example = struct
  open Base
  open Bin_prot.Std

  module V1 = struct
    type t =
      { value : int
      ; description : string list
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 154ceee89bc877b7aca43676f9ac6758 |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| e406eb2e87d821deb092e64fe9c8f01e |}]
    ;;
  end

  module V2 = struct
    type t =
      { value : int
      ; description : string list [@sexp.list]
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| 154ceee89bc877b7aca43676f9ac6758 |}]
    ;;

    (* When we add [@sexp.list], only the [sexp] changes. *)
    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 60d23dcaf6f940bccac6087fd908e2ed |}]
    ;;
  end

  (* When we add [@sexp.list], we expect the [bin_digest] to stay the same and the
     [sexp_digest] to differ. *)
  let%expect_test _ =
    let v1_bin_digest = [%bin_digest: V1.t] in
    let v2_bin_digest = [%bin_digest: V2.t] in
    Stdio.printf "%b, " (String.equal v1_bin_digest v2_bin_digest);
    let v1_sexp_digest = [%sexp_digest: V1.t] in
    let v2_sexp_digest = [%sexp_digest: V2.t] in
    Stdio.printf "%b" (String.equal v1_sexp_digest v2_sexp_digest);
    [%expect {| true, false |}]
  ;;
end

module Drop_default_example = struct
  open Base
  open Bin_prot.Std

  module V1 = struct
    type t =
      { value : int
      ; description : string
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| bf392fc686c69118afd45264a95c409a |}]
    ;;

    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 8c285ae23ce7c49b352fe9a827cf2304 |}]
    ;;
  end

  module V2 = struct
    type t =
      { value : int [@default 0] [@sexp_drop_default ( = )]
      ; description : string
      }
    [@@deriving bin_io, equal ~localize, sexp_grammar, sexp]

    let%expect_test _ =
      Stdio.print_endline [%bin_digest: t];
      [%expect {| bf392fc686c69118afd45264a95c409a |}]
    ;;

    (* When we add a [default] and [sexp_drop_default], only the [sexp] changes. *)
    let%expect_test _ =
      Stdio.print_endline [%sexp_digest: t];
      [%expect {| 56c60ebed0ed76653f9b4f038f37bad4 |}]
    ;;
  end

  (* When we add some [default] and [sexp_drop_default], we expect the [bin_digest] to
     stay the same and the [sexp_digest] to differ. *)
  let%expect_test _ =
    let v1_bin_digest = [%bin_digest: V1.t] in
    let v2_bin_digest = [%bin_digest: V2.t] in
    Stdio.printf "%b, " (String.equal v1_bin_digest v2_bin_digest);
    let v1_sexp_digest = [%sexp_digest: V1.t] in
    let v2_sexp_digest = [%sexp_digest: V2.t] in
    Stdio.printf "%b" (String.equal v1_sexp_digest v2_sexp_digest);
    [%expect {| true, false |}]
  ;;
end
