open! Core

let%expect_test "return" =
  print_s [%sexp (Common.run (Unstring.return 42) "" : int list)];
  [%expect {| (42) |}];
  print_s [%sexp (Common.run (Unstring.return 42) "anything" : int list)];
  [%expect {| () |}]
;;

let%expect_test "lit" =
  let parser = Unstring.lit "hello" in
  print_s [%sexp (Common.run parser "hello" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "world" : unit list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "hello world" : unit list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run (Unstring.lit "") "" : unit list)];
  [%expect {| (()) |}]
;;

let%expect_test "char" =
  let parser = Unstring.char (Char 'x') |> Unstring.capture in
  print_s [%sexp (Common.run parser "x" : string list)];
  [%expect {| (x) |}];
  print_s [%sexp (Common.run parser "y" : string list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "xx" : string list)];
  [%expect {| () |}]
;;

let%expect_test "char filters" =
  print_s [%sexp (Common.run (Unstring.char Any |> Unstring.capture) "z" : string list)];
  [%expect {| (z) |}];
  print_s [%sexp (Common.run (Unstring.char Digit |> Unstring.capture) "3" : string list)];
  [%expect {| (3) |}];
  print_s [%sexp (Common.run (Unstring.char Digit |> Unstring.capture) "a" : string list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run (Unstring.char Alpha |> Unstring.capture) "a" : string list)];
  [%expect {| (a) |}];
  print_s [%sexp (Common.run (Unstring.char Alpha |> Unstring.capture) "3" : string list)];
  [%expect {| () |}];
  print_s
    [%sexp (Common.run (Unstring.char Whitespace |> Unstring.capture) " " : string list)];
  [%expect {| (" ") |}];
  print_s
    [%sexp (Common.run (Unstring.char (Not Digit) |> Unstring.capture) "a" : string list)];
  [%expect {| (a) |}];
  print_s
    [%sexp (Common.run (Unstring.char (Not Digit) |> Unstring.capture) "3" : string list)];
  [%expect {| () |}]
;;

let%expect_test "char - Fun filter" =
  let parser =
    Unstring.char (Fun (fun c -> Char.equal c 'a' || Char.equal c 'b'))
    |> Unstring.capture
  in
  print_s [%sexp (Common.run parser "a" : string list)];
  [%expect {| (a) |}];
  print_s [%sexp (Common.run parser "b" : string list)];
  [%expect {| (b) |}];
  print_s [%sexp (Common.run parser "c" : string list)];
  [%expect {| () |}]
;;

let%expect_test "capture" =
  let parser = Unstring.char (Char 'a') |> Unstring.star |> Unstring.capture in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run parser "aaa" : string list)];
  [%expect {| (aaa) |}]
;;

let%expect_test "string" =
  print_s [%sexp (Common.run (Unstring.string ()) "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run (Unstring.string ()) "hello" : string list)];
  [%expect {| (hello) |}]
;;

let%expect_test "string in delimiters" =
  let open Unstring.Let_syntax in
  let parser =
    let%map () = Unstring.lit "["
    and s = Unstring.string ()
    and () = Unstring.lit "]" in
    s
  in
  print_s [%sexp (Common.run parser "[]" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run parser "[hello]" : string list)];
  [%expect {| (hello) |}];
  print_s [%sexp (Common.run parser "[a b c]" : string list)];
  [%expect {| ("a b c") |}]
;;

let%expect_test "int" =
  print_s [%sexp (Common.run Unstring.int "42" : int list)];
  [%expect {| (42) |}];
  print_s [%sexp (Common.run Unstring.int "-7" : int list)];
  [%expect {| (-7) |}];
  print_s [%sexp (Common.run Unstring.int "0" : int list)];
  [%expect {| (0) |}];
  print_s [%sexp (Common.run Unstring.int "abc" : int list)];
  [%expect {| () |}]
;;

let%expect_test "float" =
  print_s [%sexp (Common.run Unstring.float "3.14" : float list)];
  [%expect {| (3.14) |}];
  print_s [%sexp (Common.run Unstring.float "-1.5" : float list)];
  [%expect {| (-1.5) |}];
  print_s [%sexp (Common.run Unstring.float "" : float list)];
  [%expect {| () |}]
;;

let%expect_test "bool" =
  print_s [%sexp (Common.run Unstring.bool "true" : bool list)];
  [%expect {| (true) |}];
  print_s [%sexp (Common.run Unstring.bool "false" : bool list)];
  [%expect {| (false) |}];
  print_s [%sexp (Common.run Unstring.bool "True" : bool list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run Unstring.bool "yes" : bool list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run Unstring.bool "" : bool list)];
  [%expect {| () |}]
;;

let%expect_test "option" =
  let parser = Unstring.option Unstring.int ~none:(Unstring.lit "none") in
  print_s [%sexp (Common.run parser "42" : int option list)];
  [%expect {| ((42)) |}];
  print_s [%sexp (Common.run parser "-7" : int option list)];
  [%expect {| ((-7)) |}];
  print_s [%sexp (Common.run parser "none" : int option list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "other" : int option list)];
  [%expect {| () |}]
;;

let%expect_test "int - plus sign" =
  print_s [%sexp (Common.run Unstring.int "+42" : int list)];
  [%expect {| (42) |}];
  print_s [%sexp (Common.run Unstring.int "+0" : int list)];
  [%expect {| (0) |}]
;;

let%expect_test "float - special values" =
  print_s [%sexp (Common.run Unstring.float "nan" : float list)];
  [%expect {| (NAN) |}];
  print_s [%sexp (Common.run Unstring.float "inf" : float list)];
  [%expect {| (INF) |}];
  print_s [%sexp (Common.run Unstring.float "-inf" : float list)];
  [%expect {| (-INF) |}];
  print_s [%sexp (Common.run Unstring.float "+inf" : float list)];
  [%expect {| (INF) |}]
;;

let%expect_test "float - scientific notation" =
  print_s [%sexp (Common.run Unstring.float "1e10" : float list)];
  [%expect {| (10000000000) |}];
  print_s [%sexp (Common.run Unstring.float "1.5e-3" : float list)];
  [%expect {| (0.0015) |}];
  print_s [%sexp (Common.run Unstring.float "2.5E+1" : float list)];
  [%expect {| (25) |}]
;;

let%expect_test "float - edge cases" =
  print_s [%sexp (Common.run Unstring.float ".5" : float list)];
  [%expect {| (0.5) |}];
  print_s [%sexp (Common.run Unstring.float "1." : float list)];
  [%expect {| (1) |}];
  print_s [%sexp (Common.run Unstring.float "+1.5" : float list)];
  [%expect {| (1.5) |}]
;;

let%expect_test "string with char filter" =
  let parser = Unstring.string ~chars:Digit () in
  print_s [%sexp (Common.run parser "123" : string list)];
  [%expect {| (123) |}];
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run parser "123abc" : string list)];
  [%expect {| () |}]
;;

let%expect_test "all_unit" =
  let via_all_unit =
    Unstring.all_unit [ Unstring.lit "hello"; Unstring.lit " "; Unstring.lit "world" ]
  in
  let via_applicative =
    let open Unstring.Let_syntax in
    let%map () = Unstring.lit "hello"
    and () = Unstring.lit " "
    and () = Unstring.lit "world" in
    ()
  in
  print_s [%sexp (Common.run via_all_unit "hello world" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run via_applicative "hello world" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run via_all_unit "hello" : unit list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run via_applicative "hello" : unit list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run (Unstring.all_unit []) "" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run (Unstring.all_unit []) "nonempty" : unit list)];
  [%expect {| () |}]
;;

let%expect_test "capture'" =
  let open Unstring.Let_syntax in
  let parser =
    let%map () = Unstring.lit "["
    and pos, len =
      Unstring.capture'
        (Unstring.char Alpha |> Unstring.star)
        ~f:(fun () ~input:_ ~pos ~len -> pos, len)
    and () = Unstring.lit "]" in
    pos, len
  in
  print_s [%sexp (Common.run parser "[hello]" : (int * int) list)];
  [%expect {| ((1 5)) |}];
  print_s [%sexp (Common.run parser "[]" : (int * int) list)];
  [%expect {| ((1 0)) |}]
;;

let%expect_test "map" =
  let parser = Unstring.return 10 |> Unstring.map ~f:(( * ) 2) in
  print_s [%sexp (Common.run parser "" : int list)];
  [%expect {| (20) |}]
;;

let%expect_test "applicative - map2" =
  let open Unstring.Let_syntax in
  let parser =
    let%map () = Unstring.lit "("
    and digits =
      Unstring.char Digit
      |> Unstring.multiple ~bounds:(Incl 1, Unbounded)
      |> Unstring.capture
    and () = Unstring.lit ")" in
    digits
  in
  print_s [%sexp (Common.run parser "(123)" : string list)];
  [%expect {| (123) |}];
  print_s [%sexp (Common.run parser "()" : string list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "123" : string list)];
  [%expect {| () |}]
;;

let%expect_test "star" =
  let parser = Unstring.char (Char 'a') |> Unstring.star |> Unstring.capture in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run parser "a" : string list)];
  [%expect {| (a) |}];
  print_s [%sexp (Common.run parser "aaa" : string list)];
  [%expect {| (aaa) |}];
  print_s [%sexp (Common.run parser "b" : string list)];
  [%expect {| () |}]
;;

let%expect_test "multiple - bounded range" =
  let parser =
    Unstring.char (Char 'a')
    |> Unstring.multiple ~bounds:(Incl 2, Incl 4)
    |> Unstring.capture
  in
  print_s [%sexp (Common.run parser "a" : string list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "aa" : string list)];
  [%expect {| (aa) |}];
  print_s [%sexp (Common.run parser "aaa" : string list)];
  [%expect {| (aaa) |}];
  print_s [%sexp (Common.run parser "aaaa" : string list)];
  [%expect {| (aaaa) |}];
  print_s [%sexp (Common.run parser "aaaaa" : string list)];
  [%expect {| () |}]
;;

let%expect_test "multiple - with separator" =
  let parser =
    Unstring.char (Char 'x')
    |> Unstring.multiple ~sep:(Unstring.lit ",")
    |> Unstring.capture
  in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| ("") |}];
  print_s [%sexp (Common.run parser "x" : string list)];
  [%expect {| (x) |}];
  print_s [%sexp (Common.run parser "x,x,x" : string list)];
  [%expect {| (x,x,x) |}];
  print_s [%sexp (Common.run parser "x," : string list)];
  [%expect {| () |}]
;;

let%expect_test "multiple - empty range" =
  let parser =
    Unstring.char (Char 'a')
    |> Unstring.multiple ~bounds:(Incl 3, Excl 3)
    |> Unstring.capture
  in
  print_s [%sexp (Common.run parser "" : string list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "aaa" : string list)];
  [%expect {| () |}]
;;

let%expect_test "list" =
  let open Unstring.Let_syntax in
  let parser =
    let%map () = Unstring.lit "["
    and items = Unstring.list (Unstring.char Alpha |> Unstring.capture)
    and () = Unstring.lit "]" in
    items
  in
  print_s [%sexp (Common.run parser "[]" : string list list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "[abc]" : string list list)];
  [%expect {| ((a b c)) |}]
;;

let%expect_test "list - with separator" =
  let parser =
    Unstring.list ~sep:(Unstring.lit ",") (Unstring.char Alpha |> Unstring.capture)
  in
  print_s [%sexp (Common.run parser "" : string list list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "a,b,c" : string list list)];
  [%expect {| ((a b c)) |}];
  print_s [%sexp (Common.run parser "a," : string list list)];
  [%expect {| () |}]
;;

let%expect_test "list - with bounds" =
  let parser =
    Unstring.list ~bounds:(Incl 2, Incl 3) (Unstring.char (Char 'x') |> Unstring.capture)
  in
  print_s [%sexp (Common.run parser "x" : string list list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "xx" : string list list)];
  [%expect {| ((x x)) |}];
  print_s [%sexp (Common.run parser "xxx" : string list list)];
  [%expect {| ((x x x)) |}];
  print_s [%sexp (Common.run parser "xxxx" : string list list)];
  [%expect {| () |}]
;;

let%expect_test "fold" =
  let parser =
    Unstring.fold
      (Unstring.char Digit |> Unstring.capture)
      ~init:0
      ~f:(fun acc s -> acc + Int.of_string s)
  in
  print_s [%sexp (Common.run parser "" : int list)];
  [%expect {| (0) |}];
  print_s [%sexp (Common.run parser "123" : int list)];
  [%expect {| (6) |}]
;;

let%expect_test "fold - with separator and bounds" =
  let parser =
    Unstring.fold
      ~sep:(Unstring.lit ",")
      ~bounds:(Incl 1, Incl 3)
      (Unstring.char Digit |> Unstring.capture)
      ~init:0
      ~f:(fun acc s -> acc + Int.of_string s)
  in
  print_s [%sexp (Common.run parser "" : int list)];
  [%expect {| () |}];
  print_s [%sexp (Common.run parser "5" : int list)];
  [%expect {| (5) |}];
  print_s [%sexp (Common.run parser "1,2,3" : int list)];
  [%expect {| (6) |}];
  print_s [%sexp (Common.run parser "1,2,3,4" : int list)];
  [%expect {| () |}]
;;

let%expect_test "choice" =
  let parser =
    Unstring.choice [ Unstring.lit "foo"; Unstring.lit "bar"; Unstring.lit "baz" ]
  in
  print_s [%sexp (Common.run parser "foo" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "bar" : unit list)];
  [%expect {| (()) |}];
  print_s [%sexp (Common.run parser "qux" : unit list)];
  [%expect {| () |}]
;;

let%expect_test "run_unambiguous" =
  let parser = Unstring.lit "hello" in
  print_s
    [%sexp
      (Common.run_unambiguous [%sexp_of: unit] parser "hello" : unit Or_error.t option)];
  [%expect {| ((Ok ())) |}];
  print_s
    [%sexp
      (Common.run_unambiguous [%sexp_of: unit] parser "world" : unit Or_error.t option)];
  [%expect {| () |}]
;;

let%expect_test "run_unambiguous - ambiguous" =
  let parser = Unstring.choice [ Unstring.lit "a"; Unstring.lit "a" ] in
  let result = Common.run_unambiguous [%sexp_of: unit] parser "a" in
  print_s [%sexp (result : unit Or_error.t option)];
  [%expect {| ((Error ("Ambiguous parse" (first (Ok ())) (second (Ok ()))))) |}]
;;

let%expect_test "make_of_string" =
  let of_string =
    Unstring.make_of_string
      Unstring.int
      ~compile_when:`Now
      ~on_ambiguity:(`Raise [%sexp_of: _])
    |> Staged.unstage
  in
  print_s [%sexp (of_string "42" : int)];
  [%expect {| 42 |}]
;;

let%expect_test "make_of_string - no parse" =
  let of_string =
    Unstring.make_of_string
      Unstring.int
      ~compile_when:`Now
      ~on_ambiguity:(`Raise [%sexp_of: _])
    |> Staged.unstage
  in
  Expect_test_helpers_core.show_raise (fun () -> of_string "abc");
  [%expect {| (raised "Unstring: no parse found") |}]
;;

let%expect_test "make_of_string - pick_first on ambiguous" =
  let of_string =
    Unstring.make_of_string
      (Unstring.choice [ Unstring.return "first"; Unstring.return "second" ])
      ~compile_when:`Now
      ~on_ambiguity:`Pick_first
    |> Staged.unstage
  in
  print_s [%sexp (of_string "" : string)];
  [%expect {| first |}]
;;

let%expect_test "make_of_string - lazily compiled" =
  let of_string =
    Unstring.make_of_string
      (Unstring.lit "hello")
      ~compile_when:`Lazily
      ~on_ambiguity:(`Raise [%sexp_of: _])
    |> Staged.unstage
  in
  print_s [%sexp (of_string "hello" : unit)];
  [%expect {| () |}]
;;

let%expect_test "run_all_parses - error when f raises" =
  let parser =
    Unstring.map (Unstring.lit "boom") ~f:(fun () -> raise_s [%message "kaboom"])
  in
  let compiled = Unstring.compile parser in
  let results = Unstring.Compiled.run_all_parses compiled "boom" |> Sequence.to_list in
  print_s [%sexp (results : int Or_error.t list)];
  [%expect {| ((Error kaboom)) |}]
;;

let%expect_test "run_unambiguous - error when f raises" =
  let parser =
    Unstring.map (Unstring.lit "boom") ~f:(fun () -> raise_s [%message "kaboom"])
  in
  let result = Common.run_unambiguous [%sexp_of: int] parser "boom" in
  print_s [%sexp (result : int Or_error.t option)];
  [%expect {| ((Error kaboom)) |}]
;;

let%expect_test "map_hlist" =
  let parser =
    Unstring.map_hlist
      Unstring.[ int; lit ","; int ]
      ~f:(fun Heterogeneous_list.[ a; (); b ] -> a + b)
  in
  print_s [%sexp (Common.run parser "1,2" : int list)];
  [%expect {| (3) |}]
;;
