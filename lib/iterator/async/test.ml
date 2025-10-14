open! Core
open! Async

let await = Direct_async.await

let map_sequential iterator ~async ~f = exclave_
  (Iterator.map [@alloc stack]) iterator ~f:(fun x -> await async (f x))
;;

let map_parallel iterator ~async ~f = exclave_
  Iterator.map iterator ~f
  |> Iterator.cache
  |> (Iterator.map [@alloc stack]) ~f:(fun deferred -> await async deferred)
;;

let do_work n =
  let%map () = Clock_ns.after (Time_ns.Span.of_int_ms 100) in
  n * n
;;

let%expect_test "Test sequential iter" =
  Direct_async.run (fun ~async ->
    let l = List.init 10 ~f:Fn.id in
    let result =
      l |> Iterator.of_list |> map_sequential ~async ~f:do_work |> Iterator.to_list
    in
    print_s [%sexp (result : int list)];
    [%expect {| (0 1 4 9 16 25 36 49 64 81) |}])
;;

let%expect_test "Test parallel iter" =
  Direct_async.run (fun ~async ->
    let l = List.init 10 ~f:Fn.id in
    let i = Iterator.of_list l in
    let result = map_parallel i ~async ~f:do_work |> Iterator.to_list in
    print_s [%sexp (result : int list)];
    [%expect {| (0 1 4 9 16 25 36 49 64 81) |}])
;;

let%expect_test "Drive with" =
  Direct_async.run (fun ~async ->
    let l = List.init 10 ~f:Fn.id in
    let i = Iterator.of_list l in
    let driver =
      Iterator.Using_effects.Driver.create_with [ async ] (fun [ async ] -> exclave_
        map_parallel i ~async ~f:do_work)
    in
    let iter' = (Iterator.Using_effects.Driver.to_iter [@alloc stack]) driver in
    let result = Iterator.to_list iter' in
    print_s [%sexp (result : int list)];
    [%expect {| (0 1 4 9 16 25 36 49 64 81) |}])
;;
