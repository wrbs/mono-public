open! Core
open! Hardcaml
open! Hardcaml_lws
open! Bits
module Systolic_array = Hardcaml_paper_demo_rtl.Systolic_array

let n = 3
let element_width = 8

module Dut = Systolic_array.Make (struct
    let n = n
    let element_width = element_width
    let delay_inputs = true

    let instantiate_processing_element ~clocking ~enable ~left ~top =
      let open Signal in
      let contents =
        Clocking.reg_fb clocking ~enable ~width:element_width ~f:(fun fb ->
          fb +: uresize (left *: top) ~width:element_width)
      in
      let right = Clocking.reg_no_clear ~enable clocking left in
      let bottom = Clocking.reg_no_clear ~enable clocking top in
      contents, ~right, ~bottom
    ;;
  end)

module L = Hardcaml_test_harness.Lws_harness.Make (Dut.I) (Dut.O)

let run_testbench testbench = L.run ~create:Dut.create testbench

let run_matrix_multiply
  ~(a : int array array)
  ~(b : int array array)
  ~(inputs : _ Dut.I.t)
  ~(outputs : _ Dut.O.t Before_and_after_edge.t)
  (h @ local)
  =
  inputs.enable := vdd;
  (* Feed A rows from the left, B columns from the top, one column/row per cycle. *)
  for t = 0 to n - 1 do
    Array.iteri inputs.input_left ~f:(fun i port -> port <--. a.(i).(t));
    Array.iteri inputs.input_top ~f:(fun j port -> port <--. b.(t).(j));
    Lws.step h
  done;
  Array.iter inputs.input_left ~f:(fun port -> port <--. 0);
  Array.iter inputs.input_top ~f:(fun port -> port <--. 0);
  for _ = 1 to 2 * (n - 1) do
    Lws.step h
  done;
  (* Read the PE contents *)
  let result =
    Array.init n ~f:(fun i ->
      Array.init n ~f:(fun j ->
        Bits.to_unsigned_int
          !(outputs.after_edge.processing_element_contents.((i * n) + j))))
  in
  result
;;

let print_matrix ~label m =
  printf "%s:\n" label;
  Array.iter m ~f:(fun row ->
    Array.iter row ~f:(fun v -> printf "%2d " v);
    printf "\n")
;;

let software_matrix_multiply ~a ~b =
  let n = Array.length a in
  Array.init n ~f:(fun i ->
    Array.init n ~f:(fun j ->
      let sum = ref 0 in
      for k = 0 to n - 1 do
        sum := !sum + (a.(i).(k) * b.(k).(j))
      done;
      !sum))
;;

let%expect_test "3x3 matrix example" =
  let a = [| [| 1; 4; 7 |]; [| 2; 1; 3 |]; [| 1; 1; 1 |] |] in
  let b = [| [| 1; 5; 1 |]; [| 1; 1; 2 |]; [| 9; 8; 1 |] |] in
  let testbench (h @ local) ~inputs ~outputs =
    let result = run_matrix_multiply ~a ~b h ~inputs ~outputs in
    let expected = software_matrix_multiply ~a ~b in
    print_matrix ~label:"Expected" expected;
    print_endline "";
    print_matrix ~label:"Hardware result" result
  in
  run_testbench testbench;
  [%expect
    {|
    Expected:
    68 65 16
    30 35  7
    11 14  4

    Hardware result:
    68 65 16
    30 35  7
    11 14  4
    |}]
;;
