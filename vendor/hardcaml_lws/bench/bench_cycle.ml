open Hardcaml
open Hardcaml_lws

module Trivial_circuit = struct
  open Signal

  module I = struct
    type 'a t = { clock : 'a } [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { count : 'a [@bits 32] } [@@deriving hardcaml]
  end

  let create (_ : Scope.t) (i : _ I.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    { O.count = reg_fb ~width:32 spec ~f:(fun fb -> fb +:. 1) }
  ;;
end

let num_iters = 100_000

let%bench_fun "no runtime" =
  let module Sim = Cyclesim.With_interface (Trivial_circuit.I) (Trivial_circuit.O) in
  let sim =
    let scope = Scope.create ~flatten_design:true () in
    Sim.create (Trivial_circuit.create scope)
  in
  fun () ->
    for _ = 0 to num_iters - 1 do
      Cyclesim.cycle sim
    done
;;

let%bench_fun "with LWS" =
  let module L = Lws_cyclesim.With_interface (Trivial_circuit.I) (Trivial_circuit.O) in
  let testbench (h @ local) (_ : L.sim_context) =
    for _ = 0 to num_iters - 1 do
      Lws.step h
    done
  in
  fun () -> Lws_cyclesim.run (L.create Trivial_circuit.create) testbench
;;

let%bench_fun "with Step effectful" =
  let module Sim = Cyclesim.With_interface (Trivial_circuit.I) (Trivial_circuit.O) in
  let module Step = Hardcaml_step_testbench.Imperative.Cyclesim in
  let simulator =
    let scope = Scope.create ~flatten_design:true () in
    Sim.create (Trivial_circuit.create scope)
  in
  let testbench (h @ local) =
    for _ = 0 to num_iters - 1 do
      Step.cycle h
    done
  in
  fun () -> Step.run_until_finished ~simulator ~testbench ()
;;
