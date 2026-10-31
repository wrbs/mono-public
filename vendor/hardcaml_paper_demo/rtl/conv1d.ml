open Core
open Hardcaml
open Signal
module Dsl = Hardcaml_computation_dsl
module Std_funcs = Hardcaml_computation_dsl_standard_functions
module Precision = Hardcaml_computation_dsl.Precision

module Make (Config : Config.S) = struct
  include Config

  let element_width = Precision.width element_precision

  module Graph = struct
    let element_nodes =
      List.init window_size ~f:(fun i ->
        Dsl.Node.input ~label:[%string "element_%{i#Int}"] ~precision:element_precision ())
    ;;

    let kernel_nodes =
      List.init window_size ~f:(fun i ->
        Dsl.Node.input ~label:[%string "kernel_%{i#Int}"] ~precision:element_precision ())
    ;;

    let products =
      List.mapi element_nodes ~f:(fun i element ->
        let kernel = List.nth_exn kernel_nodes i in
        let product = Std_funcs.multiply element kernel in
        Std_funcs.pipeline ~label:[%string "mul_pipe_%{i#Int}"] ~n:1 product)
    ;;

    let output =
      Signal.tree products ~arity:2 ~f:(function
        | [ x ] -> x
        | [ a; b ] -> Std_funcs.pipeline ~label:"sum_pipe" ~n:1 (Std_funcs.add a b)
        | _ -> assert false)
    ;;

    let output_resized =
      Std_funcs.safe_resize ~overflow:Saturate ~precision:element_precision output
    ;;
  end

  let computation_graph = Dsl.Computation_graph.of_output_nodes [ Graph.output_resized ]

  let latency =
    let shreg_latency = 1 in
    Dsl.Computation_graph.latency computation_graph + shreg_latency
  ;;

  module Kernel = struct
    type 'a t = { weights : 'a list [@length window_size] [@bits element_width] }
    [@@deriving hardcaml]
  end

  module I = struct
    type 'a t =
      { clocking : 'a Types.Clocking.t
      ; kernel : 'a Kernel.t
      ; input_element : 'a With_valid.t [@bits element_width]
      ; clear_sliding_window : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { output_element : 'a With_valid.t [@bits element_width] }
    [@@deriving hardcaml]
  end

  let create ~build_mode (scope : Scope.t) (i : Signal.t I.t) =
    let%tydi { clocking; kernel; input_element; clear_sliding_window } = i in
    let spec = Types.Clocking.to_spec clocking in
    let shift_reg = Always.Variable.reg spec ~width:(element_width * window_size) in
    Always.(
      compile
        [ when_
            input_element.valid
            [ shift_reg
              <-- input_element.value @: drop_bottom shift_reg.value ~width:element_width
            ]
        ; when_ clear_sliding_window [ shift_reg <--. 0 ]
        ]);
    let input_signal_by_node =
      let shift_reg_elements =
        Signal.split_lsb ~part_width:element_width shift_reg.value
      in
      let element_signals = List.zip_exn Graph.element_nodes shift_reg_elements in
      let kernel_signals = List.zip_exn Graph.kernel_nodes kernel.weights in
      element_signals @ kernel_signals
    in
    let%tydi { latency = _; overflow_cnt = _; outputs; output_valid; output_overflow = _ }
      =
      Dsl.Fully_pipelined.create
        ~build_mode
        scope
        ~clocking
        ~start:(pipeline ~n:1 spec input_element.valid)
        ~input_signal_by_node
        ~computation_graph
    in
    let output_value =
      match outputs with
      | [ x ] -> x
      | _ -> failwith "Expected single output"
    in
    { O.output_element = { valid = output_valid; value = output_value } }
  ;;

  let hierarchical ~build_mode scope i =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope (create ~build_mode) i
  ;;
end
