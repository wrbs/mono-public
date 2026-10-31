open! Core

(* This file tests that the parse ordering produced by [Unstring] is stable for common
   ambiguous cases. Since [Unstring.t] is abstract, we can't instrument grammar nodes with
   indices directly (that's what the engine-level ordering tests do). Instead, we use
   concrete examples with [capture] to make the ordering visible via segment lengths.

   The ordering is "right-lexicographic" at the engine level: for a detailed formal
   treatment, see engine/test/ordering_tests.ml. These tests are not meant to be
   exhaustive; we rely on the engine ordering tests for that, and on the stability of the
   translation from [Unstring.t] into the engine language.
*)

let%expect_test "choice - branches tried in declaration order" =
  let parser =
    Unstring.choice
      [ Unstring.return "first"; Unstring.return "second"; Unstring.return "third" ]
  in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| (first second third) |}]
;;

let%expect_test "concat of star - splits ordered greedily on the left" =
  let open Unstring.Let_syntax in
  let parser =
    let%map left = Unstring.star (Unstring.char (Char 'a')) |> Unstring.capture
    and right = Unstring.star (Unstring.char (Char 'a')) |> Unstring.capture in
    String.length left, String.length right
  in
  let print input = print_s [%sexp (Common.run parser input : (int * int) list)] in
  print "a";
  print "aa";
  print "aaa";
  [%expect
    {|
    ((1 0) (0 1))
    ((2 0) (1 1) (0 2))
    ((3 0) (2 1) (1 2) (0 3))
    |}]
;;

let%expect_test "star of plus - iteration count ordering" =
  let open Unstring.Let_syntax in
  let parser =
    Unstring.list
      (let%map segment =
         Unstring.char (Char 'a')
         |> Unstring.multiple ~bounds:(Incl 1, Unbounded)
         |> Unstring.capture
       in
       String.length segment)
  in
  let print input = print_s [%sexp (Common.run parser input : int list list)] in
  print "a";
  print "aa";
  print "aaa";
  [%expect
    {|
    ((1))
    ((2) (1 1))
    ((3) (1 2) (2 1) (1 1 1))
    |}]
;;

let%expect_test "star of star - first five parses" =
  let parser =
    Unstring.list (Unstring.star (Unstring.char (Char 'a')) |> Unstring.capture)
    |> Unstring.map ~f:(List.map ~f:String.length)
  in
  let print input =
    Sequence.take (Common.run_seq parser input) 5
    |> Sequence.to_list
    |> [%sexp_of: int list list]
    |> print_s
  in
  print "a";
  print "aa";
  print "aaa";
  [%expect
    {|
    ((1) (0 1) (0 0 1) (0 0 0 1) (0 0 0 0 1))
    ((2) (0 2) (0 0 2) (0 0 0 2) (0 0 0 0 2))
    ((3) (0 3) (0 0 3) (0 0 0 3) (0 0 0 0 3))
    |}]
;;

let%expect_test "ambiguous choice in concat" =
  let open Unstring.Let_syntax in
  let parser =
    let%map left = Unstring.choice [ Unstring.return "x"; Unstring.return "y" ]
    and right = Unstring.choice [ Unstring.return "p"; Unstring.return "q" ] in
    left, right
  in
  print_s [%sexp (Common.run parser "" : (string * string) list)];
  [%expect {| ((x p) (y p) (x q) (y q)) |}]
;;

let%expect_test "right-lexicographic: rightmost choice takes priority (mli example)" =
  let open Unstring.Let_syntax in
  let parser =
    let%map left =
      Unstring.choice [ Unstring.lit "ab"; Unstring.lit "a" ] |> Unstring.capture
    and right =
      Unstring.choice [ Unstring.lit "bc"; Unstring.lit "c" ] |> Unstring.capture
    in
    left, right
  in
  print_s [%sexp (Common.run parser "abc" : (string * string) list)];
  [%expect {| ((a bc) (ab c)) |}]
;;

let%expect_test "nested choice - ordering is stable" =
  let parser =
    Unstring.choice
      [ Unstring.choice [ Unstring.return "a1"; Unstring.return "a2" ]
      ; Unstring.return "b"
      ; Unstring.choice
          [ Unstring.return "c1"; Unstring.return "c2"; Unstring.return "c3" ]
      ]
  in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| (a1 a2 b c1 c2 c3) |}]
;;
