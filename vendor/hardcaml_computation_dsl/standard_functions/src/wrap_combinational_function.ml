open! Core
open Hardcaml
open Hardcaml_computation_dsl

module Make_three_input_one_output
    (In1 : Interface_with_precision.S)
    (In2 : Interface_with_precision.S)
    (In3 : Interface_with_precision.S)
    (Out : Interface_with_precision.S) =
struct
  let wrap_function__unsafe
    ~tag
    ~latency
    fn
    (in1 : _ In1.t)
    (in2 : _ In2.t)
    (in3 : _ In3.t)
    =
    let in1_tags, in1_nodes = In1.to_alist in1 |> List.unzip in
    let in2_tags, in2_nodes = In2.to_alist in2 |> List.unzip in
    let in3_tags, in3_nodes = In3.to_alist in3 |> List.unzip in
    let inputs = in1_nodes @ in2_nodes @ in3_nodes in
    let output_tags, output_precisions = Out.to_alist Out.precision |> List.unzip in
    Node.generic_function
      ~metadata:
        { tag
        ; latency
        ; infer_output_precision = Fn.const output_precisions
        ; create =
            (fun scope ~clocking ~build_mode ~input_precisions:_ inputs ->
              let #(in1_signals, rest) = List.split_n inputs (List.length in1_tags) in
              let #(in2_signals, in3_signals) =
                List.split_n rest (List.length in2_tags)
              in
              let in1 = List.zip_exn in1_tags in1_signals |> In1.of_alist in
              let in2 = List.zip_exn in2_tags in2_signals |> In2.of_alist in
              let in3 = List.zip_exn in3_tags in3_signals |> In3.of_alist in
              let out = fn scope ~clocking ~build_mode in1 in2 in3 in
              let out_alist = Out.to_alist out in
              (* Sort `out_alist` to match the ordering in `output_tags`. *)
              List.map2_exn output_tags output_precisions ~f:(fun tag precision ->
                let q = List.Assoc.find_exn ~equal:Out.equal_tag out_alist tag in
                let expected_width = Precision.width precision in
                let actual_width = Signal.width q in
                if actual_width <> expected_width
                then
                  raise_s
                    [%message
                      "wrap_combinational_function: output signal width doesn't match \
                       expected precision"
                        (actual_width : int)
                        (expected_width : int)
                        (precision : Precision.t)];
                { Function.Data.q; overflow = Signal.gnd }))
        ; propagate_input_overflows = List.map output_precisions ~f:(Fn.const None)
        }
      ~shared:false
      inputs
    |> List.zip_exn output_tags
    |> Out.of_alist
  ;;

  let wrap_combinational_function ~tag fn in1 in2 in3 =
    wrap_function__unsafe
      ~latency:0
      ~tag
      (fun _scope ~clocking:_ ~build_mode:_ in1 in2 in3 -> fn in1 in2 in3)
      in1
      in2
      in3
  ;;
end

module Make_two_input_one_output
    (In1 : Interface_with_precision.S)
    (In2 : Interface_with_precision.S)
    (Out : Interface_with_precision.S) =
struct
  open Make_three_input_one_output (In1) (In2) (Interface_with_precision.Empty) (Out)

  let wrap_combinational_function ~tag fn in1 in2 =
    wrap_combinational_function ~tag (fun in1 in2 _ -> fn in1 in2) in1 in2 Empty
  ;;
end

module Make_one_input_one_output
    (In : Interface_with_precision.S)
    (Out : Interface_with_precision.S) =
struct
  open Make_two_input_one_output (In) (Interface_with_precision.Empty) (Out)

  let wrap_combinational_function ~tag fn i =
    wrap_combinational_function ~tag (fun i _ -> fn i) i Empty
  ;;
end

module Make_one_input_signal_output (In : Interface_with_precision.S) = struct
  let wrap_combinational_function ~tag ~output_precision fn i =
    let module Out = struct
      include Types.Scalar (struct
          let port_name = "temp_out"
          let port_width = Precision.width output_precision
        end)

      let precision = output_precision
    end
    in
    let open Make_one_input_one_output (In) (Out) in
    wrap_combinational_function ~tag (fun i -> Out.create (module Signal) (fn i)) i
    |> Out.unwrap
  ;;
end

module Make_signal_input_one_output (Out : Interface_with_precision.S) = struct
  let wrap_function__unsafe ~input_precision ~tag ~latency fn i =
    let module In = struct
      include Types.Scalar (struct
          let port_name = "temp_in"
          let port_width = Precision.width input_precision
        end)

      let precision = input_precision
    end
    in
    [%test_result: Precision.t] (Node.precision i) ~expect:In.precision;
    let open
      Make_three_input_one_output (In) (Interface_with_precision.Empty)
        (Interface_with_precision.Empty)
        (Out) in
    wrap_function__unsafe
      ~tag
      ~latency
      (fun scope ~clocking ~build_mode i _ _ ->
        let i = In.unwrap i in
        fn scope ~clocking ~build_mode i)
      (In.Unsafe.wrap i)
      Empty
      Empty
  ;;

  let wrap_combinational_function ~input_precision ~tag fn i =
    let module In = struct
      include Types.Scalar (struct
          let port_name = [%string "%{tag}_in"]
          let port_width = Precision.width input_precision
        end)

      let precision = input_precision
    end
    in
    let open Make_one_input_one_output (In) (Out) in
    [%test_result: Precision.t] (Node.precision i) ~expect:In.precision;
    wrap_combinational_function
      ~tag
      (fun i ->
        let i = In.unwrap i in
        [%test_result: int] (Signal.width i) ~expect:(Precision.width In.precision);
        fn i)
      (In.Unsafe.wrap i)
  ;;
end
