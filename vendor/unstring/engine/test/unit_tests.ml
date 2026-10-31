open! Core

let parse_and_print sexp_of grammar tokens =
  Sequence.take (Common.parse grammar tokens) 5
  |> Sequence.iter ~f:(fun r -> print_s (sexp_of r))
;;

let%expect_test "Return" =
  parse_and_print [%sexp_of: int] (Return 42) [];
  [%expect {| 42 |}]
;;

let%expect_test "Terminal - matching" =
  parse_and_print [%sexp_of: unit] (Terminal A) [ A ];
  [%expect {| () |}]
;;

let%expect_test "Terminal - non-matching" =
  parse_and_print [%sexp_of: unit] (Terminal A) [ B ];
  [%expect {| |}]
;;

let%expect_test "Map" =
  parse_and_print [%sexp_of: int] (Map (Return 10, ( * ) 2)) [];
  [%expect {| 20 |}]
;;

let%expect_test "Capture" =
  parse_and_print
    [%sexp_of: string]
    (Capture (Concat_unit [ Terminal A; Terminal B ], String.sub))
    [ A; B ];
  [%expect {| ab |}]
;;

let%expect_test "Capture_also" =
  parse_and_print
    [%sexp_of: string * int]
    (Capture_also
       ( Map (Concat_unit [ Terminal A; Terminal B ], fun () -> 99)
       , fun input ~pos ~len a -> String.sub input ~pos ~len, a ))
    [ A; B ];
  [%expect {| (ab 99) |}]
;;

let%expect_test "Union - first branch" =
  parse_and_print [%sexp_of: unit] (Union [ Terminal A; Terminal B ]) [ A ];
  [%expect {| () |}]
;;

let%expect_test "Union - second branch" =
  parse_and_print [%sexp_of: unit] (Union [ Terminal A; Terminal B ]) [ B ];
  [%expect {| () |}]
;;

let%expect_test "Union - ambiguous" =
  parse_and_print [%sexp_of: unit] (Union [ Terminal A; Terminal A ]) [ A ];
  [%expect
    {|
    ()
    ()
    |}]
;;

let%expect_test "Append" =
  parse_and_print [%sexp_of: int] (Append (Return 1, Return 2, ( + ))) [];
  [%expect {| 3 |}]
;;

let%expect_test "Append with terminals" =
  parse_and_print
    [%sexp_of: string]
    (Append (Terminal A, Terminal B, fun () () -> "matched"))
    [ A; B ];
  [%expect {| matched |}]
;;

let%expect_test "Concat_unit" =
  parse_and_print [%sexp_of: unit] (Concat_unit [ Terminal A; Terminal B ]) [ A; B ];
  [%expect {| () |}]
;;

let%expect_test "Star - empty" =
  parse_and_print
    [%sexp_of: unit list]
    (Star (Terminal A, { init = []; f = (fun acc () -> () :: acc) }))
    [];
  [%expect {| () |}]
;;

let%expect_test "Star - one iteration" =
  parse_and_print
    [%sexp_of: unit list]
    (Star (Terminal A, { init = []; f = (fun acc () -> () :: acc) }))
    [ A ];
  [%expect {| (()) |}]
;;

let%expect_test "Star - multiple iterations" =
  parse_and_print
    [%sexp_of: int]
    (Star (Terminal A, { init = 0; f = (fun acc () -> acc + 1) }))
    [ A; A; A ];
  [%expect {| 3 |}]
;;

let%expect_test "Star_unit - empty" =
  parse_and_print [%sexp_of: unit] (Star_unit (Terminal A)) [];
  [%expect {| () |}]
;;

let%expect_test "Star_unit - multiple" =
  parse_and_print [%sexp_of: unit] (Star_unit (Terminal A)) [ A; A ];
  [%expect {| () |}]
;;
