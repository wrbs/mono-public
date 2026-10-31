open Core

let%expect_test "At limits of divisor" =
  let test divisor =
    List.iter Bool.all ~f:(fun dividend_signed ->
      let divisor_bits = Hardcaml.Bits.num_bits_to_represent divisor in
      let dividend_bits = divisor_bits + 6 in
      let test =
        unstage
          (Test_compute_division_by_constants_single_divisor.single_divisor_test
             ~dividend_signed
             ~dividend_bits
             ~divisor)
      in
      let min_dividend = if dividend_signed then -(1 lsl (dividend_bits - 1)) else 0 in
      let min_quotient = Bigint.(of_int min_dividend /% of_int divisor |> to_int_exn) in
      let max_dividend =
        if dividend_signed
        then (1 lsl (dividend_bits - 1)) - 1
        else (1 lsl dividend_bits) - 1
      in
      let rec f i =
        (* This tests at the points at which the result changes (ie N*divisor plus or
           minus 1). The number of trials is bounded by [dividend_bits=divisor_bits+6]
           meaning we do couple of hundred checks but can test very large divisors. *)
        let dividend = i * divisor in
        let dividend_plus_1 = dividend + 1 in
        let dividend_plus_divisor_minus_1 = dividend + divisor - 1 in
        let test_div ~dividend =
          if dividend < min_dividend
          then None
          else if dividend > max_dividend
          then Some (Error ())
          else Some (Ok (test ~dividend))
        in
        match
          List.filter_opt
            [ test_div ~dividend
            ; test_div ~dividend:dividend_plus_1
            ; test_div ~dividend:dividend_plus_divisor_minus_1
            ]
          |> Result.all_unit
        with
        | Error () -> ()
        | Ok () -> f (i + 1)
      in
      f min_quotient)
  in
  List.iter
    [ 3
    ; 6
    ; 11
    ; 17
    ; 27
    ; 39
    ; 41
    ; 63
    ; 96
    ; 158
    ; 399
    ; 919
    ; 2_142
    ; 8_423
    ; 15_413
    ; 69_134
    ; 571_112
    ; 1_871_912
    ; 5_252_123
    ; 294_291_824
    ; 861_100_012
    ; 7_832_391_122
    ]
    ~f:test;
  [%expect {| |}]
;;
