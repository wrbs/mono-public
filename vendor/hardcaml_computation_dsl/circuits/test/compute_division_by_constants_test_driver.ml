open Hardcaml_computation_dsl
open Hardcaml
open Bits

module Make (Config : sig
    val dividend_signed : bool
    val dividend_bits : int
    val divisors : int list
  end) =
struct
  module Compute_division_by_constants =
    Hardcaml_computation_dsl_circuits.Compute_division_by_constants.Make (Config)

  module Graph = struct
    let name = "test_divide_by_constants"

    let dynamic_inputs =
      Compute_division_by_constants.Compute_q1_and_r1.I.(
        map2 port_names precision ~f:(fun label precision ->
          Node.input ~label ~precision ()))
    ;;

    let static_inputs = Hardcaml.Interface.Empty.Empty

    let output =
      Compute_division_by_constants.dsl
        ~division_multiply_cycles:0
        ~remainder_multiply_cycles:0
        ~remainder_subtraction_cycles:1
        dynamic_inputs
    ;;
  end

  module Divider_pipeline =
    Hardcaml_computation_dsl_generators.Fully_pipelined.Make
      (Compute_division_by_constants.Compute_q1_and_r1.I)
      (Interface_with_precision.Empty)
      (Compute_division_by_constants.Compute_r3.O)
      (Graph)

  let () = assert (Divider_pipeline.latency = 1)

  module Bench =
    Hardcaml_test_harness.Cyclesim_harness.Make (Divider_pipeline.I) (Divider_pipeline.O)

  let run_model = Compute_division_by_constants.model

  let run_dsl dynamic_inputs =
    Bench.run
      ~create:(Divider_pipeline.hierarchical ~build_mode:Simulation)
      (fun ~(inputs : _ Divider_pipeline.I.t) ~(outputs : _ Divider_pipeline.O.t) sim ->
         inputs.clocking.clear := vdd;
         Cyclesim.cycle sim;
         inputs.clocking.clear := gnd;
         Cyclesim.cycle sim;
         inputs.start := vdd;
         Compute_division_by_constants.Compute_q1_and_r1.I.(
           iter3
             inputs.dynamic_inputs
             dynamic_inputs
             port_widths
             ~f:(fun x dynamic_input width -> x := of_bigint_trunc dynamic_input ~width));
         Cyclesim.cycle sim;
         inputs.start := gnd;
         assert (to_bool !(outputs.valid));
         assert (not (to_bool !(outputs.overflow)));
         Compute_division_by_constants.Compute_r3.O.(
           map2 outputs.q precision ~f:(fun o precision ->
             let signedness = if precision.signed then Signedness.Signed else Unsigned in
             to_bigint !o ~signedness)))
  ;;
end
