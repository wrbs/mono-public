open! Core

type 'a t =
  | Simple of 'a Sequence.t
  | Interleaved of 'a Sequence.t list

module type Sexpable_comparable = sig
  type t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

let test
  (type a)
  (module M : Sexpable_comparable with type t = a)
  ~(actual : a Sequence.t)
  ~(expected : a t)
  ~cutoff
  : [ `Ok | `Error of Error.t | `Inconclusive ]
  =
  match expected with
  | Simple expected ->
    let actual_bounded = Sequence.length_is_bounded_by actual ~max:cutoff in
    let expected_bounded = Sequence.length_is_bounded_by expected ~max:cutoff in
    let clip x = Sequence.take x cutoff |> Sequence.to_list in
    let actual_clipped, expected_clipped = clip actual, clip expected in
    if not ([%equal: M.t list] actual_clipped expected_clipped)
    then
      `Error
        (Error.create_s
           [%message
             "Sequences not equal"
               (actual_clipped : M.t list)
               (expected_clipped : M.t list)])
    else if actual_bounded && expected_bounded
    then `Ok
    else `Inconclusive
  | Interleaved expected ->
    let actual_bounded = Sequence.length_is_bounded_by actual ~max:cutoff in
    let expected_bounded =
      Sequence.length_is_bounded_by (Sequence.concat_list expected) ~max:cutoff
    in
    let clip x = Sequence.take x cutoff |> Sequence.to_list in
    let actual_clipped = clip actual in
    let expected_clipped = List.map expected ~f:clip in
    let first_parse_check =
      match actual_clipped with
      | [] -> Ok `First_agrees
      | v :: _ ->
        if List.exists expected_clipped ~f:(fun expected ->
             [%compare.equal: M.t option] (Some v) (List.hd expected))
        then Ok `First_agrees
        else
          Or_error.error_s
            [%message "The first parse is nowhere to be found in expected parses"]
    in
    let set_check =
      let actual_set = M.Set.of_list actual_clipped in
      let expected_set = M.Set.of_list (List.concat expected_clipped) in
      match actual_bounded, expected_bounded with
      | true, true ->
        (* Both bounded: full set equality *)
        if [%compare.equal: M.Set.t] actual_set expected_set
        then Ok `Sets_equal
        else Or_error.error_s [%message "The parse sets disagree"]
      | false, false ->
        (* Both unbounded: cannot fully verify *)
        Ok `Cannot_verify
      | true, false ->
        (* Actual bounded, expected unbounded: length mismatch *)
        Or_error.error_s
          [%message "Length mismatch: actual is finite but expected is not"]
      | false, true ->
        (* Actual unbounded, expected bounded: length mismatch *)
        Or_error.error_s
          [%message "Length mismatch: actual is infinite but expected is not"]
    in
    (match first_parse_check, set_check with
     | Ok `First_agrees, Ok `Sets_equal -> `Ok
     | Ok `First_agrees, Ok `Cannot_verify -> `Inconclusive
     | Error e1, Error e2 -> `Error (Error.of_list [ e1; e2 ])
     | Error e, Ok _ | Ok _, Error e -> `Error e)
;;

module _ = struct
  module Int_comparable = struct
    include Int
    include functor Comparable.Make_plain
  end

  let%expect_test "Simple - matching sequences" =
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Simple (Sequence.of_list [ 1; 2; 3 ]) in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Ok |}]
  ;;

  let%expect_test "Simple - non-matching sequences" =
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Simple (Sequence.of_list [ 1; 2; 4 ]) in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect
      {|
      (Error
       ("Sequences not equal" (actual_clipped (1 2 3)) (expected_clipped (1 2 4))))
      |}]
  ;;

  let%expect_test "Simple - empty sequences" =
    let actual = Sequence.empty in
    let expected = Simple Sequence.empty in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Ok |}]
  ;;

  let%expect_test "Simple - respects cutoff, returns Inconclusive if unbounded" =
    let actual = Sequence.of_list [ 1; 2; 3; 4; 5 ] in
    let expected = Simple (Sequence.of_list [ 1; 2; 3; 9; 9 ]) in
    (* With cutoff=3, only first 3 elements are compared; prefixes match but both exceed
       cutoff so we get Inconclusive *)
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:3 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Inconclusive |}]
  ;;

  let%expect_test "Simple - infinite sequences with matching prefix" =
    let actual = Sequence.cycle_list_exn [ 1; 2; 3 ] in
    let expected = Simple (Sequence.cycle_list_exn [ 1; 2; 3 ]) in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Inconclusive |}]
  ;;

  let%expect_test "Simple - infinite sequences with non-matching prefix" =
    let actual = Sequence.cycle_list_exn [ 1; 2; 3 ] in
    let expected = Simple (Sequence.cycle_list_exn [ 1; 2; 4 ]) in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect
      {|
      (Error
       ("Sequences not equal"
        (actual_clipped
         (1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1
          2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2
          3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1))
        (expected_clipped
         (1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1
          2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2
          4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1 2 4 1))))
      |}]
  ;;

  let%expect_test "Interleaved - matching sets and first parse" =
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.of_list [ 1; 2 ]; Sequence.of_list [ 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Ok |}]
  ;;

  let%expect_test "Interleaved - first parse not in expected firsts" =
    (* actual first is 2, but expected firsts are 1 and 3; sets still match *)
    let actual = Sequence.of_list [ 2; 1; 3 ] in
    let expected = Interleaved [ Sequence.of_list [ 1; 2 ]; Sequence.of_list [ 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "The first parse is nowhere to be found in expected parses") |}]
  ;;

  let%expect_test "Interleaved - sets disagree" =
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.of_list [ 1; 2 ]; Sequence.of_list [ 4 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "The parse sets disagree") |}]
  ;;

  let%expect_test "Interleaved - empty actual, empty expected" =
    let actual = Sequence.empty in
    let expected = Interleaved [ Sequence.empty; Sequence.empty ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Ok |}]
  ;;

  let%expect_test "Interleaved - empty actual, non-empty expected" =
    let actual = Sequence.empty in
    let expected = Interleaved [ Sequence.of_list [ 1 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "The parse sets disagree") |}]
  ;;

  let%expect_test "Interleaved - both infinite, first parse agrees" =
    let actual = Sequence.cycle_list_exn [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.cycle_list_exn [ 1; 2; 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Inconclusive |}]
  ;;

  let%expect_test "Interleaved - both infinite, first parse disagrees" =
    (* actual starts with 9, which is not first of any expected stream *)
    let actual =
      Sequence.append (Sequence.singleton 9) (Sequence.cycle_list_exn [ 1; 2 ])
    in
    let expected = Interleaved [ Sequence.cycle_list_exn [ 1; 2; 9 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "The first parse is nowhere to be found in expected parses") |}]
  ;;

  let%expect_test "Interleaved - infinite actual, finite expected (length mismatch)" =
    let actual = Sequence.cycle_list_exn [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.of_list [ 1; 2; 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "Length mismatch: actual is infinite but expected is not") |}]
  ;;

  let%expect_test "Interleaved - finite actual, infinite expected (length mismatch)" =
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.cycle_list_exn [ 1; 2; 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| (Error "Length mismatch: actual is finite but expected is not") |}]
  ;;

  let%expect_test "Interleaved - respects cutoff for boundedness check" =
    (* With cutoff=5, sequences of length 3 are bounded *)
    let actual = Sequence.of_list [ 1; 2; 3 ] in
    let expected = Interleaved [ Sequence.of_list [ 1; 2; 3 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:5 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect {| Ok |}]
  ;;

  let%expect_test "Interleaved - both checks fail reports both errors" =
    (* first is 9 (not in expected firsts), and 9 not in expected set *)
    let actual = Sequence.of_list [ 9 ] in
    let expected = Interleaved [ Sequence.of_list [ 1 ]; Sequence.of_list [ 2 ] ] in
    let result = test (module Int_comparable) ~actual ~expected ~cutoff:100 in
    print_s [%sexp (result : [ `Ok | `Error of Error.t | `Inconclusive ])];
    [%expect
      {|
      (Error
       ("The first parse is nowhere to be found in expected parses"
        "The parse sets disagree"))
      |}]
  ;;
end
