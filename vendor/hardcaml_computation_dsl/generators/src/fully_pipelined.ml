open Core
open Hardcaml
open Hardcaml_computation_dsl

module Make
    (Dynamic_inputs : Interface_with_precision.S)
    (Static_inputs : Interface_with_precision.S)
    (Output : Interface_with_precision.S)
    (Graph : sig
       val name : string
       val dynamic_inputs : Node.t Dynamic_inputs.t
       val static_inputs : Node.t Static_inputs.t
       val output : Node.t Output.t
     end) =
struct
  let output_to_list v = Output.to_alist v |> List.map ~f:snd

  let output_of_list v =
    List.map2_exn v Output.(to_alist port_widths) ~f:(fun v (tag, _) -> tag, v)
    |> Output.of_alist
  ;;

  let computation_graph = output_to_list Graph.output |> Computation_graph.of_output_nodes
  let latency = Computation_graph.latency computation_graph

  module I = Computation_interface.Make_I (Dynamic_inputs) (Static_inputs)
  module O = Computation_interface.Make_O (Output)

  let create
    scope
    ?only_clear_counters
    ~build_mode
    ({ clocking; start; dynamic_inputs; static_inputs } : _ I.t)
    =
    let input_signal_by_node =
      let dynamic_inputs =
        Dynamic_inputs.map2 Graph.dynamic_inputs dynamic_inputs ~f:(fun node signal ->
          node, signal)
        |> Dynamic_inputs.to_list
      in
      let static_inputs =
        Static_inputs.map2 Graph.static_inputs static_inputs ~f:(fun node signal ->
          node, signal)
        |> Static_inputs.to_list
      in
      dynamic_inputs @ static_inputs
    in
    let%tydi { latency = _; overflow_cnt; outputs; output_valid; output_overflow } =
      Hardcaml_computation_dsl.Fully_pipelined.create
        ?only_clear_counters
        ~build_mode
        scope
        ~clocking
        ~start
        ~input_signal_by_node
        ~computation_graph
    in
    { O.overflow_cnt
    ; q = output_of_list outputs
    ; overflow = output_overflow
    ; valid = output_valid
    ; ready = Signal.vdd
    }
  ;;

  let hierarchical ?only_clear_counters ?instance scope ~build_mode =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical
      ?instance
      ~scope
      ~name:Graph.name
      (create ?only_clear_counters ~build_mode)
  ;;
end
