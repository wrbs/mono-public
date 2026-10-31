open Core
open Hardcaml
open Hardcaml_lws
open Expect_test_helpers_core
module Single_clock = Single_clock_domain_counter
module Dual_clock = Dual_clock_domain_counter

module%test _ = struct
  let step_n_times n (h @ local) _ = Lws.step ~n h
  let timeout = 10

  let%expect_test "cyclesim timeout - single clock" =
    let module L = Lws_cyclesim.With_interface (Single_clock.I) (Single_clock.O) in
    let run testbench =
      Lws_cyclesim.run
        (L.create
           ~config:{ Lws.Config.default with timeout = Some timeout }
           Single_clock.create)
        testbench
    in
    run (step_n_times (timeout - 1));
    [%expect {| |}];
    show_raise ~hide_positions:true (fun () -> run (step_n_times (timeout + 1)));
    [%expect {| (raised ("LWS simulation timed out" (timeout 10))) |}]
  ;;

  let%expect_test "cyclesim timeout - multiple clocks" =
    let module L = Lws_cyclesim.With_interface (Dual_clock.I) (Dual_clock.O) in
    let clock_mode =
      Cyclesim.Config.Clock_mode.By_input_clocks
        (Cyclesim_clock_domain.create_list [ "clock_a", 1; "clock_b", 2 ])
    in
    let run testbench =
      Lws_cyclesim.run
        (L.create
           ~config:{ Lws.Config.default with timeout = Some timeout }
           ~backend_specific_config:
             (Lws_cyclesim.Backend_specific_config.default_with_clock_mode clock_mode)
           Dual_clock.create)
        testbench
    in
    run (step_n_times (timeout - 1));
    [%expect {| |}];
    show_raise ~hide_positions:true (fun () -> run (step_n_times (timeout + 1)));
    [%expect {| (raised ("LWS simulation timed out" (timeout 10))) |}]
  ;;

  let%expect_test "evsim timeout - single clock" =
    let module L = Lws_evsim.With_interface (Single_clock.I) (Single_clock.O) in
    let run testbench =
      Lws_evsim.run
        (L.create
           ~config:{ Lws.Config.default with timeout = Some timeout }
           ~backend_specific_config:
             { common =
                 { clocks = [ { port_name = "clock"; time = 1; initial_delay = 0 } ]
                 ; waves = false
                 ; default_clock_name = Some "clock"
                 }
             ; backend = Lws_evsim.Evsim.Config.default
             }
           Single_clock.create)
        testbench
    in
    run (step_n_times (timeout - 1));
    [%expect {| |}];
    show_raise ~hide_positions:true (fun () -> run (step_n_times (timeout + 1)));
    [%expect {| (raised ("LWS simulation timed out" (timeout 10))) |}]
  ;;

  let%expect_test "evsim timeout - multiple clocks" =
    let module L = Lws_evsim.With_interface (Dual_clock.I) (Dual_clock.O) in
    let run testbench =
      Lws_evsim.run
        (L.create
           ~config:{ Lws.Config.default with timeout = Some timeout }
           ~backend_specific_config:
             { common =
                 { clocks =
                     [ { port_name = "clock_a"; time = 1; initial_delay = 0 }
                     ; { port_name = "clock_b"; time = 2; initial_delay = 0 }
                     ]
                 ; waves = false
                 ; default_clock_name = Some "clock_a"
                 }
             ; backend = Lws_evsim.Evsim.Config.default
             }
           Dual_clock.create)
        testbench
    in
    run (step_n_times (timeout - 1));
    [%expect {| |}];
    show_raise ~hide_positions:true (fun () -> run (step_n_times (timeout + 1)));
    [%expect {| (raised ("LWS simulation timed out" (timeout 10))) |}]
  ;;
end
