open Core
open Hardcaml_lws
open Expect_test_helpers_core
module Single_clock = Single_clock_domain_counter

module%test _ = struct
  let run_with_cyclesim testbench =
    let module L = Lws_cyclesim.With_interface (Single_clock.I) (Single_clock.O) in
    Lws_cyclesim.run (L.create Single_clock.create) testbench
  ;;

  type sim_context = Lws_context.M(Single_clock.I)(Single_clock.O).t

  let%expect_test "kill stops a running task" =
    let testbench (h @ local) (_ : sim_context) =
      let ev =
        Lws.spawn h (fun h ->
          for i = 1 to 10 do
            printf "task iteration %d\n" i;
            Lws.step h
          done)
      in
      Lws.step ~n:2 h;
      printf "killing task\n";
      Lws.Task.kill ev;
      Lws.step ~n:2 h;
      printf "done\n"
    in
    run_with_cyclesim testbench;
    [%expect
      {|
      task iteration 1
      task iteration 2
      killing task
      done
      |}]
  ;;

  let%expect_test "kill a task before it has a chance to step" =
    let testbench (h @ local) (_ : sim_context) =
      let ev =
        Lws.spawn h (fun h ->
          printf "started\n";
          Lws.step h;
          printf "this should not run\n")
      in
      printf "killing immediately\n";
      Lws.Task.kill ev;
      Lws.step ~n:2 h;
      printf "done\n"
    in
    run_with_cyclesim testbench;
    [%expect
      {|
      killing immediately
      done
      |}]
  ;;

  let%expect_test "waiting on a killed event raises" =
    let testbench (h @ local) (_ : sim_context) =
      let ev =
        Lws.spawn h (fun h ->
          for _ = 1 to 100 do
            Lws.step h
          done)
      in
      Lws.step h;
      Lws.Task.kill ev;
      Lws.wait h ev
    in
    show_raise ~hide_positions:true (fun () -> run_with_cyclesim testbench);
    [%expect
      {|
      (raised (
        "An exception occurred when stepping the LWS simulator. I have stepped all the children."
        ((
          (spawned_at lib/hardcaml/lws/test/test_kill.ml:LINE:COL)
          (exn        "Lws.wait: task was killed")
          (backtrace ())))))
      |}]
  ;;

  let%expect_test "killing an already-resolved event is a no-op" =
    let testbench (h @ local) (_ : sim_context) =
      let ev = Lws.spawn h (fun _h -> 42) in
      Lws.step h;
      printf "result before kill: %d\n" (Lws.wait h ev);
      Lws.Task.kill ev;
      printf
        "state: %s\n"
        (Sexp.to_string_hum [%sexp (Lws.Task.state ev : int Lws.Task.State.t)]);
      printf "result after kill: %d\n" (Or_null.value_exn (Lws.Task.read ev))
    in
    run_with_cyclesim testbench;
    [%expect
      {|
      result before kill: 42
      state: (Resolved 42)
      result after kill: 42
      |}]
  ;;

  let%expect_test "killing a task does not affect its sibling tasks" =
    let testbench (h @ local) (_ : sim_context) =
      let ev_a =
        Lws.spawn h (fun h ->
          for i = 1 to 5 do
            printf "task A iteration %d\n" i;
            Lws.step h
          done)
      in
      Lws.spawn_unit h (fun h ->
        for i = 1 to 5 do
          printf "task B iteration %d\n" i;
          Lws.step h
        done);
      Lws.step ~n:2 h;
      printf "killing task A\n";
      Lws.Task.kill ev_a;
      Lws.step ~n:4 h;
      printf "done\n"
    in
    run_with_cyclesim testbench;
    [%expect
      {|
      task A iteration 1
      task B iteration 1
      task A iteration 2
      task B iteration 2
      killing task A
      task B iteration 3
      task B iteration 4
      task B iteration 5
      done
      |}]
  ;;
end
