open! Core

(** Small, targeted quickcheck properties for individual [Unstring] combinators.

    Unlike the [equivalence_tests] which use [Blueprint] to generate arbitrary parser
    trees, these tests exercise specific combinators with simple, concrete generators
    (short strings, small bounds, etc.) to verify their basic semantics. *)

let matches parser str = not (List.is_empty (Common.run parser str))

let%expect_test "lit s matches exactly s" =
  let%quick_test prop (s : string) (other : string) =
    assert (matches (Unstring.lit s) s);
    if not (String.equal s other) then assert (not (matches (Unstring.lit s) other))
  in
  [%expect {| |}]
;;

let%expect_test "star (char c) matches homogeneous strings and rejects foreign characters"
  =
  let%quick_test prop
    (c : char)
    (other : char)
    (n : (int[@generator Quickcheck.Generator.small_non_negative_int]))
    =
    let parser = Unstring.star (Unstring.char (Char c)) in
    assert (matches parser (String.make n c));
    if not (Char.equal c other)
    then assert (not (matches parser (String.make n c ^ String.make 1 other)))
  in
  [%expect {| |}]
;;

let%expect_test "choice [lit a; lit b; ...] matches iff input is one of the alternatives" =
  let%quick_test prop (alternatives : string list) (input : string) =
    let choice_of alts = Unstring.choice (List.map alts ~f:Unstring.lit) in
    [%test_eq: bool]
      (List.mem alternatives input ~equal:String.equal)
      (matches (choice_of alternatives) input);
    assert (matches (choice_of (input :: alternatives)) input)
  in
  [%expect {| |}]
;;

let%expect_test "map2 (lit a) (lit b) matches exactly a ^ b" =
  let%quick_test prop (a : string) (b : string) (other : string) =
    let parser = Unstring.map2 (Unstring.lit a) (Unstring.lit b) ~f:(fun () () -> ()) in
    assert (matches parser (a ^ b));
    if not (String.equal other (a ^ b)) then assert (not (matches parser other))
  in
  [%expect {| |}]
;;

let is_in_bounds ~lo ~hi n =
  Maybe_bound.is_lower_bound lo ~of_:n ~compare:Int.compare
  && Maybe_bound.is_upper_bound hi ~of_:n ~compare:Int.compare
;;

let check_list_parse ~parser ~n ~lo ~hi input =
  let result = Common.run_unambiguous [%sexp_of: string list] parser input in
  if is_in_bounds ~lo ~hi n
  then (
    match result with
    | Some (Ok items) -> [%test_eq: int] (List.length items) n
    | Some (Error e) -> Error.raise e
    | None ->
      raise_s
        [%message
          "Expected a parse but got none"
            (n : int)
            (lo : int Maybe_bound.t)
            (hi : int Maybe_bound.t)
            (input : string)])
  else [%test_eq: string list Or_error.t option] result None
;;

let%expect_test "list ~sep ~bounds collects the right number of items" =
  let%quick_test prop
    (lo : (int[@generator Quickcheck.Generator.small_non_negative_int]) Maybe_bound.t)
    (hi : (int[@generator Quickcheck.Generator.small_non_negative_int]) Maybe_bound.t)
    (n : (int[@generator Quickcheck.Generator.small_non_negative_int]))
    =
    let parser =
      Unstring.list
        ~sep:(Unstring.lit ",")
        ~bounds:(lo, hi)
        (Unstring.char (Char 'x') |> Unstring.capture)
    in
    let input =
      if n = 0 then "" else List.init n ~f:(fun _ -> "x") |> String.concat ~sep:","
    in
    check_list_parse ~parser ~n ~lo ~hi input
  in
  [%expect {| |}]
;;

let%expect_test "list ~bounds (no sep) collects the right number of items" =
  let%quick_test prop
    (lo : (int[@generator Quickcheck.Generator.small_non_negative_int]) Maybe_bound.t)
    (hi : (int[@generator Quickcheck.Generator.small_non_negative_int]) Maybe_bound.t)
    (n : (int[@generator Quickcheck.Generator.small_non_negative_int]))
    =
    let parser =
      Unstring.list ~bounds:(lo, hi) (Unstring.char (Char 'x') |> Unstring.capture)
    in
    let input = String.make n 'x' in
    check_list_parse ~parser ~n ~lo ~hi input
  in
  [%expect {| |}]
;;
