open! Core

let show sexp_of collection =
  let sexps = collection |> Collector.(collect list) |> List.map ~f:sexp_of in
  print_s [%sexp (sexps : Sexp.t list)]
;;

let show_int = show [%sexp_of: int]

let%expect_test "from list containers" =
  let input = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let test collection =
    (* Test fold *)
    show_int collection;
    [%expect {| (1 2 3 4 5 6 7 8) |}];
    (* Test fold_until too *)
    let first_3 = Collection.take collection ~n:3 in
    show_int first_3;
    [%expect {| (1 2 3) |}]
  in
  (* The expect test above checks all containers have the same output *)
  test (input |> Collection.list);
  test (input |> Sequence.of_list |> Collection.sequence);
  test (input |> Set.of_list (module Int) |> Collection.set);
  test (input |> Vec.of_list |> Collection.vec);
  test (input |> Array.of_list |> Collection.array);
  test (input |> Iarray.of_list |> Collection.iarray);
  test (input |> Queue.of_list |> Collection.queue);
  test (input |> Stack.of_list |> Collection.stack)
;;

let%expect_test "hash sets" =
  let input = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  (* Hash sets are different, but we can make sure they're stable *)
  input
  |> Hash_set.of_list (module Int)
  |> Collection.hash_set
  |> Collector.(collect list)
  |> List.sort ~compare:[%compare: int]
  |> [%sexp_of: int list]
  |> print_s;
  [%expect {| (1 2 3 4 5 6 7 8) |}];
  input
  |> Hash_set.of_list (module Int)
  |> Collection.hash_set
  |> Collection.take ~n:3
  |> Collector.(collect count)
  |> [%sexp_of: int]
  |> print_s;
  [%expect {| 3 |}]
;;

let%expect_test "other creators" =
  (* empty*)
  show_int Collection.empty;
  [%expect {| () |}];
  (* singleton *)
  show_int (Collection.singleton 42);
  [%expect {| (42) |}];
  (* unfold *)
  show
    [%sexp_of: string]
    (Collection.unfold "" ~f:(fun s ->
       if String.length s < 10
       then (
         let s' = s ^ "X" in
         Some (s', s))
       else None));
  [%expect {| ("" X XX XXX XXXX XXXXX XXXXXX XXXXXXX XXXXXXXX XXXXXXXXX) |}];
  (* repeatedly_call *)
  let c = Collection.repeatedly_call ~f:(fun s -> s ^ "X") ~on:"" in
  show [%sexp_of: string] (Collection.take c ~n:5);
  [%expect {| ("" X XX XXX XXXX) |}];
  (* Ranges *)
  show_int (Collection.range 0 5);
  [%expect {| (0 1 2 3 4) |}];
  show_int (Collection.range 0 5 ~start:`exclusive ~stop:`inclusive);
  [%expect {| (1 2 3 4 5) |}];
  show_int (Collection.range 0 5 ~stride:2);
  [%expect {| (0 2 4) |}];
  show_int (Collection.range 0 5 ~start:`exclusive ~stop:`inclusive ~stride:2);
  [%expect {| (2 4) |}];
  show_int (Collection.range 5 0 ~stride:(-2));
  [%expect {| (5 3 1) |}];
  show_int (Collection.range 5 0 ~start:`exclusive ~stop:`inclusive ~stride:(-2));
  [%expect {| (3 1) |}];
  ()
;;

let%expect_test "forever" =
  show_int Collection.(forever (list [ 1; 2 ]) |> take ~n:5);
  [%expect {| (1 2 1 2 1) |}]
;;

let%expect_test "cons" =
  show_int Collection.(cons 1 (list [ 2; 3 ]));
  [%expect {| (1 2 3) |}]
;;

let%expect_test "concat" =
  show_int
    Collection.(
      concat
        (list
           [ list [ 1; 2 ]
           ; array [| 3 |]
           ; list []
           ; list [ 4; 5; 6; 7; 8 ]
           ; iarray [: 9; 10; 11; 12 :]
           ]));
  [%expect {| (1 2 3 4 5 6 7 8 9 10 11 12) |}]
;;

(* TODO: get ai for rest *)
