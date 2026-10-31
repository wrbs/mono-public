open Core
open Hardcaml
open Hardcaml_lws
module Dut = Single_clock_domain_counter
module L = Lws_cyclesim.With_interface (Dut.I) (Dut.O)

type sim_context = L.sim_context

module Make_tests (M : sig
    val simulation_backend : Hardcaml_verilator.Simulation_backend.t
  end) =
struct
  let run testbench =
    Lws_cyclesim.run
      (L.create
         ~backend_specific_config:
           { Lws_cyclesim.Backend_specific_config.default with
             simulation_backend = M.simulation_backend
           }
         Dut.create)
      testbench
  ;;

  let%expect_test "Use the run api with verilator backend" =
    let testbench (h @ local) ({ inputs; outputs; _ } : sim_context) =
      let ev =
        Lws.spawn h (fun h ->
          for _ = 0 to 5 - 1 do
            Lws.step h;
            printf "Hello!\n"
          done)
      in
      for _ = 1 to 4 do
        inputs.enable := Bits.( ~: ) !(inputs.enable);
        Lws.step h;
        printf "%d\n" (Bits.to_unsigned_int !(outputs.after_edge.count))
      done;
      Lws.wait h ev
    in
    run testbench;
    [%expect
      {|
      1
      Hello!
      1
      Hello!
      2
      Hello!
      2
      Hello!
      Hello!
      |}]
  ;;

  let%expect_test "Using the evaluator API with verilator backend" =
    let module L = Lws_cyclesim.With_interface (Dut.I) (Dut.O) in
    let lws =
      L.create
        ~backend_specific_config:
          { Lws_cyclesim.Backend_specific_config.default with
            simulation_backend = Verilator No_cache
          }
        Dut.create
    in
    let trigger =
      Lws_cyclesim.resolve_trigger_by_clock_name lws "clock" |> Option.value_exn
    in
    let ev1 =
      Lws_cyclesim.schedule_task ~trigger lws (fun h _ ->
        for _ = 0 to 5 do
          Lws.step h;
          printf "Hello!\n"
        done)
    in
    let ev2 =
      Lws_cyclesim.schedule_task ~trigger lws (fun h sim_context ->
        for _ = 1 to 4 do
          sim_context.inputs.enable := Bits.( ~: ) !(sim_context.inputs.enable);
          Lws.step h;
          printf "%d\n" (Bits.to_unsigned_int !(sim_context.outputs.after_edge.count))
        done)
    in
    Lws_cyclesim.poll_task lws ev1;
    Lws_cyclesim.poll_task lws ev2;
    [%expect
      {|
      Hello!
      1
      Hello!
      1
      Hello!
      2
      Hello!
      2
      Hello!
      Hello!
      |}]
  ;;
end

module%test With_hardcaml_cyclesim = Make_tests (struct
    let simulation_backend = Hardcaml_verilator.Simulation_backend.Hardcaml
  end)

module%test With_hardcaml_verilator = Make_tests (struct
    let simulation_backend = Hardcaml_verilator.Simulation_backend.Verilator No_cache
  end)
