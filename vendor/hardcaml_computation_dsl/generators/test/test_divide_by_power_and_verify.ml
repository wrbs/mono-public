open Core
open Tester.Make (Divide_by_power_and_verify)

(*=
   {[
     let%expect_test "Dump the dotfile" =
       Divide_by_power_and_verify.For_debug.print ();;
   }]
*)

let%expect_test "Several handwritten tests with overflows, non-multiple ticks, and \
                 positive and negative powers."
  =
  print_s [%message (Divide_by_power_and_verify.latency : int)];
  [%expect {| (Divide_by_power_and_verify.latency 8) |}];
  run_test
    ~verbose:true
    ~waves:false
    ~dynamic_inputs_list:
      [ { pow = 3.; dividend = 125. }
      ; { pow = 4.; dividend = 250. }
      ; { pow = 4.; dividend = 625. }
      ; { pow = 1.; dividend = 10. }
      ; { pow = 1.; dividend = 11. }
      ; { pow = 0.; dividend = 123. }
      ; { pow = -1.; dividend = 11. }
      ; { pow = -1.; dividend = Float.of_int (1 lsl 25) }
      ]
    ~static_inputs:Empty
    ();
  [%expect
    {|
    ((step_output
      ((q ((quotient (1)) (verify_division (1)))) (overflow (false)) (cycle 9)))
     (expect
      ((q ((quotient (1)) (verify_division (1)))) (overflow (false)) (cycle 9))))
    ((step_output
      ((q ((quotient (26843546)) (verify_division (0)))) (overflow (false))
       (cycle 10)))
     (expect
      ((q ((quotient ()) (verify_division (0)))) (overflow ()) (cycle 10))))
    ((step_output
      ((q ((quotient (1)) (verify_division (1)))) (overflow (false)) (cycle 11)))
     (expect
      ((q ((quotient (1)) (verify_division (1)))) (overflow (false)) (cycle 11))))
    ((step_output
      ((q ((quotient (2)) (verify_division (1)))) (overflow (false)) (cycle 12)))
     (expect
      ((q ((quotient (2)) (verify_division (1)))) (overflow (false)) (cycle 12))))
    ((step_output
      ((q ((quotient (-53687089)) (verify_division (0)))) (overflow (false))
       (cycle 13)))
     (expect
      ((q ((quotient ()) (verify_division (0)))) (overflow ()) (cycle 13))))
    ((step_output
      ((q ((quotient (123)) (verify_division (1)))) (overflow (false))
       (cycle 14)))
     (expect
      ((q ((quotient (123)) (verify_division (1)))) (overflow (false))
       (cycle 14))))
    ((step_output
      ((q ((quotient (55)) (verify_division (1)))) (overflow (false)) (cycle 15)))
     (expect
      ((q ((quotient (55)) (verify_division (1)))) (overflow (false)) (cycle 15))))
    ((step_output
      ((q ((quotient (33554432)) (verify_division (1)))) (overflow (true))
       (cycle 16)))
     (expect
      ((q ((quotient ()) (verify_division ()))) (overflow (true)) (cycle 16))))
    |}]
;;

let quickcheck ?(min_num_computations = 50) ?(max_num_computations = 100) ~trials () =
  let open Quickcheck.Generator.Let_syntax in
  let inputs =
    let%bind static_inputs = Quickcheck_generators.static_inputs in
    let%bind dynamic_inputs_list =
      Int.gen_incl min_num_computations max_num_computations
      >>= Fn.flip List.gen_with_length Quickcheck_generators.dynamic_inputs
    in
    return (~static_inputs, ~dynamic_inputs_list)
  in
  Quickcheck.test ~trials inputs ~f:(fun (~static_inputs, ~dynamic_inputs_list) ->
    run_test ~static_inputs ~dynamic_inputs_list ())
;;

let%expect_test "quickcheck" =
  quickcheck ~trials:20 ();
  [%expect {| |}]
;;
