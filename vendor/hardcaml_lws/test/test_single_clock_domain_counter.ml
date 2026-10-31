open Core
open Hardcaml
open Hardcaml_lws
module Dut = Single_clock_domain_counter

module%test _ = struct
  let run_with_cyclesim testbench =
    let module Lws_backend = Lws_cyclesim in
    let module L = Lws_backend.With_interface (Dut.I) (Dut.O) in
    Lws_backend.run (L.create Dut.create) testbench
  ;;

  let run_with_evsim testbench =
    let module Lws_backend = Lws_evsim in
    let module L = Lws_backend.With_interface (Dut.I) (Dut.O) in
    Lws_backend.run
      (L.create
         ~backend_specific_config:
           { common =
               { clocks = [ { port_name = "clock"; time = 1; initial_delay = 0 } ]
               ; waves = true
               ; default_clock_name = Some "clock"
               }
           ; backend = Lws_backend.Evsim.Config.default
           }
         Dut.create)
      testbench
  ;;

  type sim_context = Lws_context.M(Dut.I)(Dut.O).t

  let%expect_test "Use the run api" =
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
    List.iter [ run_with_cyclesim; run_with_evsim ] ~f:(fun run ->
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
        |}])
  ;;

  let%expect_test "Demonstrate ordering of spawn events" =
    let testbench (h @ local) _ =
      let rec f trace (h @ local) =
        for i = 0 to 1 do
          let trace_str =
            String.concat ~sep:" " (List.rev (List.map ~f:Int.to_string (i :: trace)))
          in
          printf "%s\n" trace_str;
          Lws.step h;
          ignore (Lws.spawn h (fun h -> f (i :: trace) h) : unit Lws.Task.t)
        done
      in
      f [] h
    in
    List.iter [ run_with_cyclesim; run_with_evsim ] ~f:(fun run ->
      run testbench;
      [%expect
        {|
        0
        1
        0 0
        0 1
        1 0
        0 0 0
        |}])
  ;;

  let%expect_test "step with n=0 is a no-op" =
    let testbench (h @ local) ({ inputs; outputs; _ } : sim_context) =
      inputs.enable := Bits.vdd;
      Lws.step ~n:1 h;
      printf
        "after step ~n:1 count=%d\n"
        (Bits.to_unsigned_int !(outputs.after_edge.count));
      Lws.step ~n:0 h;
      printf
        "after step ~n:0 count=%d\n"
        (Bits.to_unsigned_int !(outputs.after_edge.count));
      Lws.step ~n:1 h;
      printf
        "after step ~n:1 count=%d\n"
        (Bits.to_unsigned_int !(outputs.after_edge.count))
    in
    List.iter [ run_with_cyclesim; run_with_evsim ] ~f:(fun run ->
      run testbench;
      [%expect
        {|
        after step ~n:1 count=1
        after step ~n:0 count=1
        after step ~n:1 count=2
        |}])
  ;;

  let%expect_test "Using the evaluator API" =
    let test (lws : sim_context Lws_runtime.t) =
      let trigger =
        Lws_runtime.resolve_trigger_by_clock_name lws "clock" |> Option.value_exn
      in
      let ev1 =
        Lws_runtime.schedule_task ~trigger lws (fun h _ ->
          for _ = 0 to 5 do
            Lws.step h;
            printf "Hello!\n"
          done)
      in
      let ev2 =
        Lws_runtime.schedule_task ~trigger lws (fun h sim_context ->
          for _ = 1 to 4 do
            sim_context.inputs.enable := Bits.( ~: ) !(sim_context.inputs.enable);
            Lws.step h;
            printf "%d\n" (Bits.to_unsigned_int !(sim_context.outputs.after_edge.count))
          done)
      in
      Lws_runtime.poll_task lws ev1;
      Lws_runtime.poll_task lws ev2;
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
    in
    let () =
      let module L_cyclesim = Lws_cyclesim.With_interface (Dut.I) (Dut.O) in
      test (Lws_cyclesim.to_polymorphic (L_cyclesim.create Dut.create))
    in
    let () =
      let module L_evsim = Lws_evsim.With_interface (Dut.I) (Dut.O) in
      let lws =
        L_evsim.create
          ~backend_specific_config:
            { common =
                { clocks = [ { port_name = "clock"; time = 1; initial_delay = 0 } ]
                ; waves = false
                ; default_clock_name = Some "clock"
                }
            ; backend = Lws_evsim.Evsim.Config.default
            }
          Dut.create
      in
      test (Lws_evsim.to_polymorphic lws)
    in
    [%expect {| |}]
  ;;
end
