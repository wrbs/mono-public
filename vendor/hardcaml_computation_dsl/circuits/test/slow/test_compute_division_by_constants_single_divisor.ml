open Core

open
  Hardcaml_computation_dsl_circuits_test.Test_compute_division_by_constants_single_divisor

let%expect_test "random trials" =
  let dividends_per_trial = 1000 in
  let trials = 1000 in
  let max_divisor = 100000 in
  Quickcheck.test
    ~trials
    (let%bind.Quickcheck dividend_signed = Bool.quickcheck_generator in
     let%bind.Quickcheck divisor = Int.gen_incl 1 max_divisor in
     let divisor_bits = Int.ceil_log2 (divisor + 1) in
     let%bind.Quickcheck dividend_bits =
       Int.gen_incl
         (if dividend_signed then divisor_bits + 1 else divisor_bits)
         (divisor_bits + 20)
     in
     let%map.Quickcheck dividends =
       if dividend_signed
       then (
         let min_dividend = -(1 lsl (dividend_bits - 1)) in
         let max_dividend = (1 lsl (dividend_bits - 1)) - 1 in
         List.gen_with_length dividends_per_trial (Int.gen_incl min_dividend max_dividend))
       else
         List.gen_with_length
           dividends_per_trial
           (Int.gen_incl 0 ((1 lsl dividend_bits) - 1))
     in
     ~dividend_signed, ~divisor, ~dividend_bits, ~dividends)
    ~f:(fun (~dividend_signed, ~divisor, ~dividend_bits, ~dividends) ->
      let test = unstage (single_divisor_test ~dividend_signed ~dividend_bits ~divisor) in
      List.iter dividends ~f:(fun dividend -> test ~dividend));
  [%expect {| |}]
;;
