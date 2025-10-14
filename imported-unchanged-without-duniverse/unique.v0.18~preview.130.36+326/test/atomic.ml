open! Core

type t : value mod contended portable = { foo : string }

let mk_unique : string @ unique -> t @ unique = fun foo -> { foo }

let consume_unique (t @ portable) =
  print_string "Consumed unique: ";
  print_endline t.foo
;;

let in_capsule (f @ portable) = f ()

let%expect_test "atomic make, get" =
  let t1 = mk_unique "hello" in
  let r = Unique.Atomic.make t1 in
  in_capsule (fun () ->
    let t2 = Unique.Atomic.get r in
    consume_unique t2);
  [%expect {| Consumed unique: hello |}]
;;

let%expect_test "atomic exchange, set" =
  let r = Unique.Atomic.make (mk_unique "original") in
  in_capsule (fun () ->
    let old = Unique.Atomic.exchange r (mk_unique "replacement") in
    consume_unique old);
  [%expect {| Consumed unique: original |}];
  in_capsule (fun () -> Unique.Atomic.set r (mk_unique "set value"));
  let final = Unique.Atomic.exchange r (mk_unique "another replacement") in
  consume_unique final;
  [%expect {| Consumed unique: set value |}]
;;
