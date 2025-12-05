open! Core
open! Midi

let%expect_test "Values roundtrip" =
  List.iter Value.all ~f:(fun value ->
    [%test_eq: Value.t] value (value |> Value.to_byte |> Value.of_byte_exn);
    [%test_eq: Value.t] value (value |> Value.to_int |> Value.of_int_exn))
;;

let test_double_roundtrips value =
  let ~hi, ~lo = Value.Double.to_values value in
  let value' = Value.Double.of_values ~hi ~lo in
  [%test_eq: Value.Double.t] value value'
;;

let%expect_test "Double values roundtrip through values" =
  Quickcheck.test
    [%quickcheck.generator: Value.Double.t]
    ~sexp_of:[%sexp_of: Value.Double.t]
    ~f:test_double_roundtrips
;;

let%expect_test "Double value ends" =
  let small = Value.Double.of_values ~hi:Value.min_value ~lo:Value.min_value in
  [%test_eq: Value.Double.t] small Value.Double.min_value;
  test_double_roundtrips small;
  let big = Value.Double.of_values ~hi:Value.max_value ~lo:Value.max_value in
  [%test_eq: Value.Double.t] big Value.Double.max_value;
  test_double_roundtrips big
;;
