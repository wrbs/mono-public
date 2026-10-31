open! Core
open Expect_test_helpers_core

(* Testing portable memo in a single domain *)

let%expect_test "Portable_memo.t" =
  let memo = Portable_memo.create (module Int) in
  let find key =
    let res = Portable_memo.find memo key in
    print_s [%sexp (res : int option)]
  in
  let add ~key ~data =
    let res = Portable_memo.add memo ~key ~data in
    print_s [%sexp (res : int)]
  in
  find 0;
  [%expect {| () |}];
  add ~key:0 ~data:1;
  [%expect {| 1 |}];
  find 0;
  [%expect {| (1) |}];
  add ~key:0 ~data:2;
  [%expect {| 1 |}]
;;

let%expect_test "Portable_memo.memo" =
  let total_number_of_computations = Atomic.make 0 in
  let f =
    Portable_memo.memo (module Int) (fun x ->
      Atomic.incr total_number_of_computations;
      x)
  in
  let run x =
    let result = f x in
    let total_num = Atomic.get total_number_of_computations in
    print_s [%message (result : int) (total_num : int)]
  in
  run 0;
  [%expect
    {|
    ((result    0)
     (total_num 1))
    |}];
  run 0;
  [%expect
    {|
    ((result    0)
     (total_num 1))
    |}];
  run 1;
  [%expect
    {|
    ((result    1)
     (total_num 2))
    |}];
  run 0;
  [%expect
    {|
    ((result    0)
     (total_num 2))
    |}];
  run 1;
  [%expect
    {|
    ((result    1)
     (total_num 2))
    |}];
  run 2;
  [%expect
    {|
    ((result    2)
     (total_num 3))
    |}]
;;

let%expect_test "cache hits don't allocate" =
  let f = Portable_memo.memo (module Int) (fun x -> Int.to_string x) in
  print_s [%sexp (f 12345 : string)];
  [%expect {| 12345 |}];
  print_s [%sexp (require_no_allocation (fun () -> f 12345) : string)];
  [%expect {| 12345 |}]
;;
