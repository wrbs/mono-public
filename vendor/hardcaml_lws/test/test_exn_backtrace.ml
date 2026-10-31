open Core
open Hardcaml_lws
module Dut = Single_clock_domain_counter
module L = Lws_cyclesim.With_interface (Dut.I) (Dut.O)

let run tb = Lws_cyclesim.run (L.create Dut.create) tb

let sanitize_output init =
  (* The expect test collector relies on the following 3 strings (case sensitive) to
     decide to print a CR. So, we suppress it by uppercasing their occurence in the expect
     test output.
  *)
  [ "Raised at "; "Called from "; "Raised by primitive operation " ]
  |> List.fold ~init ~f:(fun s pattern ->
    String.substr_replace_all ~pattern ~with_:(String.uppercase pattern) s)
  |> Expect_test_helpers_core.hide_positions_in_string
;;

let test_this_must_raise tb =
  try
    run (fun (h @ local) _ -> tb h);
    Expect_test_helpers_core.print_cr
      [%message "SOMETHING IS REALLY WRONG DON'T PRINT ME!"]
  with
  | exn -> print_string (sanitize_output (Exn.to_string exn))
;;

(* All the following tests calls [test_exn_backtrace_testbench.raise_and_die] to kill the
   testbench. The tests' expect test backtrace output should should include this function.
*)

let%expect_test "main task crashes" =
  test_this_must_raise (fun (h @ local) -> Test_exn_backtrace_testbench.raise_and_die h);
  [%expect
    {|
    ("An exception occurred when stepping the LWS simulator. I have stepped all the children."
      (((spawned_at lib/hardcaml/lws/test/test_exn_backtrace.ml:LINE:COL)
         (exn (Failure "I AM DED!"))
         (backtrace
           (("RAISED AT Stdlib.failwith in file \"stdlib.ml\" (inlined), line LINE, characters C1-C2"
              "CALLED FROM Hardcaml_lws_test__Test_exn_backtrace_testbench.raise_and_die in file \"test_exn_backtrace_testbench.ml\", line LINE, characters C1-C2"))))))
    |}]
;;

let%expect_test "spawned task crashes when main task is stepping." =
  test_this_must_raise (fun (h @ local) ->
    Lws.spawn_unit h (fun h -> Test_exn_backtrace_testbench.raise_and_die h);
    Lws.step ~n:2 h);
  [%expect
    {|
    ("An exception occurred when stepping the LWS simulator. I have stepped all the children."
      (((spawned_at lib/hardcaml/lws/test/test_exn_backtrace.ml:LINE:COL)
         (exn (Failure "I AM DED!"))
         (backtrace
           (("RAISED AT Stdlib.failwith in file \"stdlib.ml\" (inlined), line LINE, characters C1-C2"
              "CALLED FROM Hardcaml_lws_test__Test_exn_backtrace_testbench.raise_and_die in file \"test_exn_backtrace_testbench.ml\", line LINE, characters C1-C2"))))))
    |}]
;;

let%expect_test "spawned task crashes when main task is waiting for it." =
  test_this_must_raise (fun (h @ local) ->
    let ev =
      Lws.spawn h (fun h ->
        Lws.step h;
        Test_exn_backtrace_testbench.raise_and_die h)
    in
    Lws.wait h ev);
  [%expect
    {|
    ("An exception occurred when stepping the LWS simulator. I have stepped all the children."
      (((spawned_at lib/hardcaml/lws/test/test_exn_backtrace.ml:LINE:COL)
         (exn (Failure "I AM DED!"))
         (backtrace
           (("RAISED AT Stdlib.failwith in file \"stdlib.ml\" (inlined), line LINE, characters C1-C2"
              "CALLED FROM Hardcaml_lws_test__Test_exn_backtrace_testbench.raise_and_die in file \"test_exn_backtrace_testbench.ml\", line LINE, characters C1-C2"))))))
    |}]
;;

let%expect_test "spawned task crashes when a different spawned task is waiting for it." =
  test_this_must_raise (fun (h @ local) ->
    let ev =
      Lws.spawn h (fun h ->
        Lws.step h;
        Test_exn_backtrace_testbench.raise_and_die h)
    in
    Lws.spawn_unit h (fun h -> Lws.wait h ev);
    Lws.step h);
  [%expect
    {|
    ("An exception occurred when stepping the LWS simulator. I have stepped all the children."
      (((spawned_at lib/hardcaml/lws/test/test_exn_backtrace.ml:LINE:COL)
         (exn (Failure "I AM DED!"))
         (backtrace
           (("RAISED AT Stdlib.failwith in file \"stdlib.ml\" (inlined), line LINE, characters C1-C2"
              "CALLED FROM Hardcaml_lws_test__Test_exn_backtrace_testbench.raise_and_die in file \"test_exn_backtrace_testbench.ml\", line LINE, characters C1-C2"))))))
    |}]
;;
