open! Core
open! Hardcaml
open! Hardcaml_home_networking

module type Repr = sig
  type t [@@deriving string, sexp, compare, quickcheck]
end

let check_roundtrip (type t) (module M : Repr with type t = t) t =
  [%test_eq: M.t] t (t |> M.to_string |> M.of_string)
;;

let%expect_test "Basic roundtrips" =
  check_roundtrip (module Addr.Ip.Const) (Addr.Ip.Const.of_string "0.0.0.0");
  check_roundtrip (module Addr.Ip.Const) (Addr.Ip.Const.of_string "255.255.255.255");
  check_roundtrip (module Addr.Ip.Const) (Addr.Ip.Const.of_string "0.1.2.3")
;;

let%expect_test "Basic roundtrips" =
  check_roundtrip (module Addr.Mac.Const) (Addr.Mac.Const.of_string "00:00:00:00:00:00");
  check_roundtrip (module Addr.Mac.Const) (Addr.Mac.Const.of_string "ff:ff:ff:ff:ff:ff");
  check_roundtrip (module Addr.Mac.Const) (Addr.Mac.Const.of_string "01:23:45:67:89:ab")
;;
