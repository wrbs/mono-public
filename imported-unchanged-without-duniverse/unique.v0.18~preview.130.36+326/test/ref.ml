open! Core

type t = { foo : string }

let mk_unique : string @ unique -> t @ unique = fun foo -> { foo }
let consume_unique (t @ unique) = Printf.printf "Consumed unique: %s\n" t.foo

let%expect_test "ref make, get" =
  let t1 = mk_unique "hello" in
  let r = Unique.Ref.make t1 in
  let t2 = Unique.Ref.get r in
  consume_unique t2;
  [%expect {| Consumed unique: hello |}]
;;

let%expect_test "ref exchange, set" =
  let r = Unique.Ref.make (mk_unique "original") in
  let old = Unique.Ref.exchange r (mk_unique "replacement") in
  consume_unique old;
  [%expect {| Consumed unique: original |}];
  Unique.Ref.set r (mk_unique "set value");
  let final = Unique.Ref.exchange r (mk_unique "another replacement") in
  consume_unique final;
  [%expect {| Consumed unique: set value |}]
;;
