open! Core
open! Hardcaml
open! Hardcaml_lws
module Conv1d = Hardcaml_paper_demo_rtl.Conv1d
module Config = Hardcaml_paper_demo_rtl.Config
module Fixed_point_utils = Hardcaml_paper_demo_driver.Fixed_point_utils
module Precision = Hardcaml_computation_dsl.Precision

module Make (Config : Config.S) = struct
  module Conv = Conv1d.Make (Config)
  module L = Hardcaml_test_harness.Lws_harness.Make (Conv.I) (Conv.O)

  let run_testbench testbench =
    L.run ~create:(Conv.create ~build_mode:Simulation) testbench
  ;;

  let float_to_fixed flt =
    Fixed_point_utils.float_to_fixed_bits
      Config.element_precision
      (Unboxed.F64.of_float flt)
  ;;

  let fixed_to_float f =
    Unboxed.F64.to_float
      (Fixed_point_utils.fixed_bits_to_float Config.element_precision f)
  ;;

  let validate_output_values ~kernel ~input_values ~output_values =
    let expected_outputs =
      Software_model.conv1d Config.element_precision ~kernel ~input:input_values
    in
    (* Verify they match (with some tolerance for fixed-point rounding) *)
    let hw_outputs = Vec.to_list output_values in
    List.iter2_exn hw_outputs expected_outputs ~f:(fun hw expected ->
      if Float.( > ) (Float.abs (hw -. expected)) 0.01
      then printf "MISMATCH: hw=%.4f expected=%.4f\n" hw expected)
  ;;

  let print_output_values output_values =
    printf "Hardware outputs:\n";
    Vec.iteri output_values ~f:(fun i v -> printf "  Output %d: %.4f\n" (i + 1) v)
  ;;

  let run_single_test_example
    ~kernel_values
    ~input_values
    ~print_output
    (h @ local)
    ~(inputs : Bits.t ref Conv.I.t)
    ~(outputs : Bits.t ref Conv.O.t Before_and_after_edge.t)
    =
    let output_values = Vec.create () in
    (* Set kernel weights (constant for this test) *)
    List.iter2_exn inputs.kernel.weights kernel_values ~f:(fun port weight ->
      port := float_to_fixed weight);
    Lws.spawn_unit h (fun h ->
      List.iter input_values ~f:(fun v ->
        inputs.input_element.value := float_to_fixed v;
        inputs.input_element.valid := Bits.vdd;
        Lws.step h);
      inputs.input_element.valid := Bits.gnd);
    let collector =
      Lws.spawn h (fun h ->
        let num_inputs = List.length input_values in
        for _ = 1 to num_inputs + Conv.latency + 2 do
          Lws.step h;
          if Bits.to_bool !(outputs.after_edge.output_element.valid)
          then
            Vec.push_back
              output_values
              (fixed_to_float !(outputs.after_edge.output_element.value))
        done)
    in
    Lws.wait h collector;
    if print_output then print_output_values output_values;
    print_endline "";
    validate_output_values ~kernel:kernel_values ~input_values ~output_values
  ;;

  let test_single_example ~kernel_values ~input_values =
    assert (List.length kernel_values = Config.window_size);
    printf "Window size: %d\n" Config.window_size;
    printf "Latency: %d\n" Conv.latency;
    printf "\n";
    let testbench (h @ local) ~inputs ~outputs =
      run_single_test_example
        ~print_output:true
        ~kernel_values
        ~input_values
        ~inputs
        ~outputs
        h
    in
    run_testbench testbench
  ;;

  let gen_input_vector ~max_num_input_elements_to_test =
    let open Quickcheck.Generator.Let_syntax in
    let%bind input_vector_length = Int.gen_incl 1 max_num_input_elements_to_test in
    List.gen_with_length
      input_vector_length
      (Fixed_point_utils.generate_float Config.element_precision)
  ;;

  let generate_test_case ~max_num_input_elements_to_test_per_kernel =
    let open Quickcheck.Generator.Let_syntax in
    let%bind kernel_values =
      List.gen_with_length
        Config.window_size
        (Fixed_point_utils.generate_float Config.element_precision)
    in
    let%bind input_values =
      gen_input_vector
        ~max_num_input_elements_to_test:max_num_input_elements_to_test_per_kernel
    in
    return (~kernel_values, ~input_values)
  ;;

  let test_quickcheck ~num_kernels_to_test ~max_num_input_elements_to_test_per_kernel =
    Quickcheck.test
      (generate_test_case ~max_num_input_elements_to_test_per_kernel)
      ~trials:num_kernels_to_test
      ~f:(fun (~kernel_values, ~input_values) ->
        run_testbench
          (run_single_test_example ~kernel_values ~input_values ~print_output:false))
  ;;
end

module%test Test_case_1 = struct
  include Make (struct
      let window_size = 3
      let element_precision = { Precision.signed = true; i_bits = 8; f_bits = 8 }
    end)

  let%expect_test "basic" =
    test_single_example
      ~kernel_values:[ 0.25; 0.5; 0.25 ]
      ~input_values:[ 1.0; 2.0; 3.0; 4.0; 5.0 ];
    [%expect
      {|
      Window size: 3
      Latency: 4

      Hardware outputs:
        Output 1: 0.2500
        Output 2: 1.0000
        Output 3: 2.0000
        Output 4: 3.0000
        Output 5: 4.0000
      |}]
  ;;

  let%expect_test "quickcheck" =
    test_quickcheck ~num_kernels_to_test:10 ~max_num_input_elements_to_test_per_kernel:100;
    [%expect {| |}]
  ;;
end

module%test Test_case_2 = struct
  include Make (struct
      let window_size = 5
      let element_precision = { Precision.signed = true; i_bits = 16; f_bits = 16 }
    end)

  let%expect_test "basic" =
    test_single_example
      ~kernel_values:[ 0.1; 0.2; 0.4; 0.2; 0.1 ]
      ~input_values:[ 1.0; 2.0; 3.0; 4.0; 5.0; 6.0; 7.0 ];
    [%expect
      {|
      Window size: 5
      Latency: 5

      Hardware outputs:
        Output 1: 0.1000
        Output 2: 0.4000
        Output 3: 1.1000
        Output 4: 2.0000
        Output 5: 3.0000
        Output 6: 4.0000
        Output 7: 5.0000
      |}]
  ;;

  let%expect_test "quickcheck" =
    test_quickcheck ~num_kernels_to_test:10 ~max_num_input_elements_to_test_per_kernel:100;
    [%expect {| |}]
  ;;
end
