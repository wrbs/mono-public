open! Core

(** Tests that the built-in parsers ([int], [float], [bool], [string]) roundtrip with
    their corresponding [to_string] functions. *)

let make_of_string unstring =
  Unstring.make_of_string
    unstring
    ~compile_when:`Lazily
    ~on_ambiguity:(`Raise [%sexp_of: _])
  |> Staged.unstage
;;

let bool_of_string = make_of_string Unstring.bool
let int_of_string = make_of_string Unstring.int
let float_of_string = make_of_string Unstring.float
let string_of_string = make_of_string (Unstring.string ())

let%expect_test "int roundtrips with Int.to_string" =
  let%quick_test prop (i : int) =
    [%test_result: int] (int_of_string (Int.to_string i)) ~expect:i
  in
  [%expect {| |}]
;;

let%expect_test "float roundtrips with Float.to_string" =
  let%quick_test prop (f : float) =
    [%test_result: float] (float_of_string (Float.to_string f)) ~expect:f
  in
  [%expect {| |}]
;;

let%expect_test "bool roundtrips with Bool.to_string" =
  let%quick_test prop (b : bool) =
    [%test_result: bool] (bool_of_string (Bool.to_string b)) ~expect:b
  in
  [%expect {| |}]
;;

let%expect_test "string roundtrips" =
  let%quick_test prop (s : string) =
    [%test_result: string] (string_of_string s) ~expect:s
  in
  [%expect {| |}]
;;

let int_list_of_string =
  make_of_string (Unstring.list ~sep:(Unstring.lit ",") Unstring.int)
;;

let float_list_of_string =
  make_of_string (Unstring.list ~sep:(Unstring.lit ",") Unstring.float)
;;

let%expect_test "int list roundtrips" =
  let%quick_test prop (is : int list) =
    [%test_result: int list]
      (int_list_of_string (List.map is ~f:Int.to_string |> String.concat ~sep:","))
      ~expect:is
  in
  [%expect {| |}]
;;

let%expect_test "float list roundtrips" =
  let%quick_test prop (fs : float list) =
    [%test_result: float list]
      (float_list_of_string (List.map fs ~f:Float.to_string |> String.concat ~sep:","))
      ~expect:fs
  in
  [%expect {| |}]
;;
