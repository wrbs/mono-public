open Core
open Expect_test_helpers_core

let single_divisor_test ~dividend_signed ~dividend_bits ~divisor =
  let open Compute_division_by_constants_test_driver.Make (struct
      let dividend_signed = dividend_signed
      let dividend_bits = dividend_bits
      let divisors = [ divisor ]
    end) in
  let open Compute_division_by_constants in
  let coefficients =
    get_coefficients ~divisor |> Option.value_exn |> Coefficients.map ~f:Bigint.of_int
  in
  stage (fun ~dividend ->
    let dividend = Bigint.of_int dividend in
    let expect = For_testing.expect ~dividend ~divisor:(Bigint.of_int divisor) in
    let i = { Compute_q1_and_r1.I.dividend; coefficients } in
    let model_result = run_model i in
    let dsl_result = run_dsl i in
    [%test_result: Bigint.t Compute_r3.O.t] model_result ~expect;
    [%test_result: Bigint.t Compute_r3.O.t] dsl_result ~expect)
;;

let%expect_test "Exhaustive over a few ranges" =
  let exhaustive dividend_signed dividend_bits divisor =
    let test = unstage (single_divisor_test ~dividend_signed ~dividend_bits ~divisor) in
    if dividend_signed
    then (
      let max_dividend = (1 lsl (dividend_bits - 1)) - 1 in
      let min_dividend = -(1 lsl (dividend_bits - 1)) in
      for dividend = min_dividend to max_dividend do
        test ~dividend
      done)
    else
      for dividend = 0 to (1 lsl dividend_bits) - 1 do
        test ~dividend
      done
  in
  exhaustive false 8 3;
  exhaustive false 5 20;
  exhaustive false 10 33;
  exhaustive false 9 97;
  exhaustive true 8 3;
  exhaustive true 6 20;
  exhaustive true 10 33;
  exhaustive true 9 97
;;

let%expect_test "divisor larger than dividend " =
  require_does_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:false ~dividend_bits:3 ~divisor:8
       : (dividend:int -> unit) Staged.t));
  [%expect
    {|
    ("Divisor is larger than largest possible dividend"
     (unsigned_dividend_bits 3)
     (divisor                8))
    |}];
  require_does_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:true ~dividend_bits:4 ~divisor:8
       : (dividend:int -> unit) Staged.t));
  [%expect
    {|
    ("Divisor is larger than largest possible dividend"
     (unsigned_dividend_bits 3)
     (divisor                8))
    |}];
  [%expect {| |}];
  require_does_not_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:false ~dividend_bits:3 ~divisor:7
       : (dividend:int -> unit) Staged.t));
  [%expect {| |}];
  require_does_not_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:true ~dividend_bits:4 ~divisor:7
       : (dividend:int -> unit) Staged.t));
  [%expect {| |}];
  require_does_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:false ~dividend_bits:30 ~divisor:(1 lsl 30)
       : (dividend:int -> unit) Staged.t));
  [%expect
    {|
    ("Divisor is larger than largest possible dividend"
     (unsigned_dividend_bits 30)
     (divisor                1073741824))
    |}];
  require_does_raise (fun () ->
    ignore
      (single_divisor_test ~dividend_signed:true ~dividend_bits:31 ~divisor:(1 lsl 30)
       : (dividend:int -> unit) Staged.t));
  [%expect
    {|
    ("Divisor is larger than largest possible dividend"
     (unsigned_dividend_bits 30)
     (divisor                1073741824))
    |}];
  require_does_not_raise (fun () ->
    ignore
      (single_divisor_test
         ~dividend_signed:false
         ~dividend_bits:30
         ~divisor:((1 lsl 30) - 1)
       : (dividend:int -> unit) Staged.t));
  [%expect {| |}];
  require_does_not_raise (fun () ->
    ignore
      (single_divisor_test
         ~dividend_signed:true
         ~dividend_bits:31
         ~divisor:((1 lsl 30) - 1)
       : (dividend:int -> unit) Staged.t));
  [%expect {| |}]
;;

let loop_with_stride test start stop stride =
  let rec loop p =
    if p > stop
    then ()
    else (
      test ~dividend:p;
      loop (p + stride))
  in
  loop start
;;

let%expect_test "Print constants using a realistic divisor and dividend bits." =
  let dividend_bits = 50 in
  let divisor = 10_000 in
  let open Compute_division_by_constants_test_driver.Make (struct
      let dividend_signed = false
      let dividend_bits = dividend_bits
      let divisors = [ divisor ]
    end) in
  let coefficients =
    Compute_division_by_constants.get_coefficients ~divisor |> Option.value_exn
  in
  print_s [%message (coefficients : int Compute_division_by_constants.Coefficients.t)];
  [%expect
    {|
    (coefficients (
      (n        4)
      (divisor' 625)
      (m        115292150460685)
      (k        56)))
    |}];
  print_s
    [%message
      (Compute_division_by_constants.Compute_q1_and_r1.I.port_widths
       : int Compute_division_by_constants.Compute_q1_and_r1.I.t)];
  [%expect
    {|
    (Compute_division_by_constants.Compute_q1_and_r1.I.port_widths
     ((dividend 50)
      (coefficients (
        (n        3)
        (divisor' 10)
        (m        47)
        (k        6)))))
    |}];
  print_s
    [%message
      (Compute_division_by_constants.Compute_q1_and_r1.O.port_widths
       : int Compute_division_by_constants.Compute_q1_and_r1.O.t)];
  [%expect
    {|
    (Compute_division_by_constants.Compute_q1_and_r1.O.port_widths
     ((unsigned_q1 46)
      (q1          46)
      (r1          4)
      (dividend_msb ())
      (coefficients (
        (n        3)
        (divisor' 10)
        (m        47)
        (k        6)))))
    |}];
  print_s
    [%message
      (Compute_division_by_constants.Compute_q2.O.port_widths
       : int Compute_division_by_constants.Compute_q2.O.t)];
  [%expect
    {|
    (Compute_division_by_constants.Compute_q2.O.port_widths
     ((q1 46)
      (r1 4)
      (q2 37)
      (coefficients (
        (n        3)
        (divisor' 10)
        (m        47)
        (k        6)))))
    |}];
  print_s
    [%message
      (Compute_division_by_constants.Compute_r3.O.port_widths
       : int Compute_division_by_constants.Compute_r3.O.t)];
  [%expect
    {|
    (Compute_division_by_constants.Compute_r3.O.port_widths
     ((q2 37)
      (r3 14)))
    |}]
;;

let%expect_test "Systematically check a bunch of numbers at the top of the range, using \
                 a realistic divisor and dividend bits."
  =
  let dividend_bits = 50 in
  let divisor = 10_000 in
  let test =
    unstage (single_divisor_test ~dividend_signed:false ~dividend_bits ~divisor)
  in
  let start = 1 lsl (dividend_bits - 5) in
  let stop = (1 lsl dividend_bits) - 1 in
  let stride = 1 lsl (dividend_bits - 10) in
  loop_with_stride test start stop stride;
  loop_with_stride test (start - 1) stop stride;
  loop_with_stride test (start - 15) stop stride;
  [%expect {| |}]
;;
