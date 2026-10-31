open Core
open Hardcaml
open Hardcaml_computation_dsl

module Make (X : Interface_with_precision.S) = struct
  module W = Wrap_combinational_function.Make_signal_input_one_output (X)

  let create values ~latency ~read_address =
    let address_precision =
      Signal.address_bits_for (Array.length values) |> Precision.unsigned_int
    in
    [%test_result: Precision.t] (Node.precision read_address) ~expect:address_precision;
    W.wrap_function__unsafe
      ~tag:"rom"
      ~input_precision:address_precision
      ~latency
      (fun _scope ~clocking ~build_mode:_ read_address ->
        let values = Array.map values ~f:X.Of_bits.pack in
        (Signal.rom ~read_addresses:[| read_address |] values).(0)
        |> Clocking.pipeline_no_clear ~n:latency clocking
        |> X.Of_signal.unpack)
      read_address
  ;;
end
