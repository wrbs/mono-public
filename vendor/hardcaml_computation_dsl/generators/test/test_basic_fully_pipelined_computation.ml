open Core
open Tester.Make (Basic_fully_pipelined_computation)

let%expect_test "basic test" =
  print_s [%message (Basic_fully_pipelined_computation.latency : int)];
  [%expect {| (Basic_fully_pipelined_computation.latency 2) |}];
  run_test
    ~verbose:true
    ~dynamic_inputs_list:
      [ { base = 3.; input0 = 2.; input1 = 0.5; input2 = 2.; input3 = 0.5; input6 = -2. }
      ]
    ~static_inputs:
      { Basic_fully_pipelined_computation.Static_inputs.scale0 = 1.
      ; scale1 = 1.
      ; offset1 = 1.
      ; shift2 = 1.
      ; shift3 = 2.
      }
    ();
  [%expect
    {|
    ((step_output
      ((q
        ((q1 (5)) (q2 (3.5)) (q3 (7.125)) (q4 (1)) (q5 (0)) (q6 (30)) (q7 (0))
         (q8 (0.5)) (q9 (0.5)) (q10 (0)) (q11 (0)) (q12 (0)) (q13_valid (1))
         (q13_value (3)) (q14 (2)) (q15 (2)) (q16 (0)) (q17 (5)) (q18 (-1.5))
         (q19 (6))))
       (overflow (true)) (cycle 3)))
     (expect
      ((q
        ((q1 (5)) (q2 (3.5)) (q3 (7.125)) (q4 (1)) (q5 (0)) (q6 ()) (q7 (0))
         (q8 (0.5)) (q9 (0.5)) (q10 (0)) (q11 (0)) (q12 (0)) (q13_valid (1))
         (q13_value (3)) (q14 (2)) (q15 (2)) (q16 (0)) (q17 (5)) (q18 (-1.5))
         (q19 (6))))
       (overflow (true)) (cycle 3))))
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
  quickcheck ~trials:10 ();
  [%expect {| |}]
;;
