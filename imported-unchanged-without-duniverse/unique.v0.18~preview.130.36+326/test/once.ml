open! Core

type f = unit -> string @ unique

let mk_once : string @ unique -> f @ once portable = fun s () -> s
let consume_once (f : f) = Printf.printf "Consumed: %s\n" (f ())

let%expect_test "once make, get" =
  let f1 = Unique.Once.make (mk_once "hello") in
  let f2 = Unique.Once.make (mk_once "world") in
  consume_once (Unique.Once.get_exn f1);
  [%expect {| Consumed: hello |}];
  consume_once (Unique.Once.get_exn f2);
  [%expect {| Consumed: world |}]
;;

let%expect_test "once get_or_null" =
  let consume f =
    match Unique.Once.get_or_null f with
    | This v -> consume_once v
    | Null -> print_endline "Null"
  in
  let f1 = Unique.Once.make (mk_once "only accessible once") in
  let f2 = Unique.Once.make (mk_once "also only accessible once") in
  consume f1;
  [%expect {| Consumed: only accessible once |}];
  consume f2;
  [%expect {| Consumed: also only accessible once |}];
  consume f2;
  [%expect {| Null |}];
  consume f1;
  [%expect {| Null |}]
;;

let%expect_test "once get twice" =
  let consume f =
    match Unique.Once.get_or_null f with
    | This v -> consume_once v
    | Null -> print_endline "Null"
  in
  let consume' f =
    match Unique.Once.get_exn f with
    | v -> consume_once v
    | exception Failure s -> print_endline s
  in
  let f1 = Unique.Once.make (mk_once "only accessible once") in
  let f2 = Unique.Once.make (mk_once "also only accessible once") in
  let f3 = Unique.Once.make (mk_once "and also only accessible once") in
  consume f1;
  [%expect {| Consumed: only accessible once |}];
  consume' f2;
  [%expect {| Consumed: also only accessible once |}];
  consume' f3;
  [%expect {| Consumed: and also only accessible once |}];
  consume f2;
  [%expect {| Null |}];
  consume' f1;
  [%expect {| Once.get_exn failed: already accessed |}];
  consume' f3;
  [%expect {| Once.get_exn failed: already accessed |}]
;;

let%expect_test "once atomic get twice" =
  let consume f =
    match Unique.Once.Atomic.get_opt f with
    | Some v -> consume_once v
    | None -> print_endline "None"
  in
  let consume' f =
    match Unique.Once.Atomic.get_exn f with
    | v -> consume_once v
    | exception Failure s -> print_endline s
  in
  let f1 = Unique.Once.Atomic.make (mk_once "only accessible once") in
  let f2 = Unique.Once.Atomic.make (mk_once "also only accessible once") in
  let f3 = Unique.Once.Atomic.make (mk_once "and also only accessible once") in
  consume f1;
  [%expect {| Consumed: only accessible once |}];
  consume' f2;
  [%expect {| Consumed: also only accessible once |}];
  consume' f3;
  [%expect {| Consumed: and also only accessible once |}];
  consume f2;
  [%expect {| None |}];
  consume' f1;
  [%expect {| Once.get_exn failed: already accessed |}];
  consume' f3;
  [%expect {| Once.get_exn failed: already accessed |}]
;;
