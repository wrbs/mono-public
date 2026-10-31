open Core
open Hardcaml
open Unboxed
module Precision = Hardcaml_computation_dsl.Precision

let float_to_fixed_int (precision : Precision.t) f =
  F64.(iround_nearest_exn (f * F64.of_int (1 lsl precision.f_bits)))
;;

let fixed_int_to_float (precision : Precision.t) signed_val =
  F64.(of_int signed_val / (#2. ** of_int precision.f_bits))
;;

let float_to_fixed_bits (precision : Precision.t) f =
  Bits.of_signed_int ~width:(Precision.width precision) (float_to_fixed_int precision f)
;;

let fixed_bits_to_float (precision : Precision.t) bits : f64 =
  assert (Bits.width bits = Precision.width precision);
  fixed_int_to_float precision (Bits.to_signed_int bits)
;;

let generate_float (precision : Precision.t) =
  let open Quickcheck.Generator.Let_syntax in
  let element_width = Precision.width precision in
  let max_representable_value_as_int = (1 lsl (element_width - 1)) - 1 in
  let min_representable_value_as_int = -(1 lsl (element_width - 1)) in
  let gen_positive = Int.gen_log_incl 0 max_representable_value_as_int in
  let gen_negative =
    let%map x = Int.gen_log_incl 1 (-min_representable_value_as_int) in
    Int.neg x
  in
  let%map x =
    Quickcheck.Generator.weighted_union
      [ 0.02, Quickcheck.Generator.return 0; 0.49, gen_positive; 0.49, gen_negative ]
  in
  F64.to_float (fixed_int_to_float precision x)
;;

let%expect_test "Demonstrate distribution of generated values" =
  let random = Splittable_random.create Random.State.default in
  let p = { Precision.signed = true; f_bits = 8; i_bits = 8 } in
  let values =
    Quickcheck.Generator.generate
      ~size:1
      ~random
      (List.gen_with_length 10 (generate_float p))
  in
  List.iter values ~f:(fun x -> printf "%f\n" x);
  [%expect
    {|
    0.000000
    2.121094
    0.027344
    0.128906
    -57.054688
    -0.007812
    58.050781
    0.011719
    0.011719
    -9.625000
    |}]
;;
