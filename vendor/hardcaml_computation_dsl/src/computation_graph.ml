open Core
open Hardcaml
module Clocking = Clocking
module Cycle = Int

module Start_and_end_cycles = struct
  type t =
    { start_cycle : Cycle.t
    ; end_cycle : Cycle.t
    }
  [@@deriving sexp_of, fields ~getters]

  module Option = struct
    type nonrec t = t option

    let to_string = function
      | None -> "No start/end"
      | Some { start_cycle; end_cycle } ->
        [%string "(start, end)=(%{start_cycle#Int}, %{end_cycle#Int})"]
    ;;
  end
end

(* Internal metadata that we track per node *)
module Node_metadata = struct
  type t =
    { endpoints : Start_and_end_cycles.t option
    ; mutable output : Signal.t Function.Data.t list
    (* Empty when the graph is initially constructed; populated during the [wire_up]
       phase. For [Function] nodes, this list has one element per output. For all other
       node types, it has exactly one element. *)
    }
  [@@deriving fields ~getters]
end

module By_node = struct
  type 'a t = 'a Int.Table.t

  let get_node t node = Hashtbl.find_exn t (Node.id node)
  let set_node t node data = Hashtbl.set t ~key:(Node.id node) ~data
end

type t =
  { outputs : Node.t list
  ; latency : int
  ; internal_by_node : Node_metadata.t By_node.t
  }

let all_parents_are_constant ~internal_by_node ~node =
  let parents = Node.parents node in
  if List.is_empty parents
  then false
  else
    List.for_all parents ~f:(fun parent ->
      match parent with
      | Input _ | Constant _ -> Node.is_constant parent
      | Function _ | Function_output_buffer _ ->
        (* A Function or Function_output_buffer is effectively constant if it has no
           endpoints (i.e., all of its own parents are constant). *)
        Option.is_none (By_node.get_node internal_by_node parent).Node_metadata.endpoints)
;;

let construct_internal_of_node ~internal_by_node ~node =
  let parent_internals =
    Node.parents node (* Find the end cycles of all the parents *)
    |> List.map ~f:(By_node.get_node internal_by_node)
  in
  let endpoints =
    if all_parents_are_constant ~internal_by_node ~node
    then
      (* If all the parents are constant, this node is also treated as a constant. *)
      None
    else (
      let start_cycle =
        parent_internals (* Find the end cycles of all the parents *)
        (* Filter out any parents with [end_cycle = None] - this means they are always
           valid. *)
        |> List.filter_map ~f:Node_metadata.endpoints
        (* Find the parent with the latest [end_cycle]. *)
        |> List.map ~f:Start_and_end_cycles.end_cycle
        |> List.max_elt ~compare:Int.compare
        (* If there are no parents, then the start cycle is 0. *)
        |> Option.value ~default:0
      in
      match Node.latency node with
      | None -> None
      | Some v -> Some { Start_and_end_cycles.start_cycle; end_cycle = start_cycle + v })
  in
  { Node_metadata.endpoints; output = [] }
;;

let get_node_metadata ~internal_by_node ~node = By_node.get_node internal_by_node node

let latency_of_node ~(internal_by_node : Node_metadata.t By_node.t) ~node =
  Option.map
    (get_node_metadata ~internal_by_node ~node).endpoints
    ~f:(fun { end_cycle; _ } -> end_cycle)
;;

let map_certain_elements list ~select ~map_list =
  let tagged = List.map list ~f:(fun x -> x, select x) in
  let selected = List.filter_map tagged ~f:(fun (x, b) -> if b then Some x else None) in
  let mapped = map_list selected in
  let rec merge tagged mapped =
    match tagged, mapped with
    | [], [] -> []
    | [], _ -> failwith "map_list returned more elements than expected"
    | (x, false) :: rest, mapped -> x :: merge rest mapped
    | (_, true) :: rest, m :: mapped_rest -> m :: merge rest mapped_rest
    | (_, true) :: _, [] -> failwith "map_list returned fewer elements than expected"
  in
  merge tagged mapped
;;

(* Construct a graph from the given output nodes, annotating all of the relevant
   information. The final output will be fixed latency, and correspond to the latency of
   the slowest output - the remaining outputs get pipelined to that latency. *)
let of_output_nodes outputs =
  let internal_by_node = Int.Table.create () in
  let construct_node node =
    let internal = construct_internal_of_node ~internal_by_node ~node in
    By_node.set_node internal_by_node node internal
  in
  (* Do the initial traversal of the entire graph, discovered from all output nodes. *)
  Node.postorder_traversal outputs ~f:construct_node;
  (* Compute the total output latency. *)
  let latency =
    List.fold ~init:None outputs ~f:(fun acc node ->
      let node_latency = latency_of_node ~internal_by_node ~node in
      match acc, node_latency with
      | None, None -> None
      | None, Some latency -> Some latency
      | Some acc, None -> Some acc
      | Some acc, Some latency -> Some (Int.max acc latency))
  in
  if Option.is_none latency
  then
    raise_s
      [%message "At least one output in the computation graph must be non-constant!"];
  let latency = Option.value_exn latency in
  (* Construct pipelining for the output nodes that need it and traverse these nodes. *)
  let aligned_outputs =
    map_certain_elements
      outputs (* We only align non-constant nodes. *)
      ~select:(fun node -> Option.is_some (latency_of_node ~internal_by_node ~node))
        (* We align the non-contant nodes by constructing a no-op function node that takes
           them all as inputs - the auto-pipelining feature will align all the inputs. *)
      ~map_list:(fun nodes ->
        let alignment_nodes =
          Node.generic_function
            ~metadata:
              { tag = "output_alignment_node"
              ; latency = 0
              ; infer_output_precision = Fn.id
              ; create =
                  (fun _scope
                    ~clocking:_
                    ~build_mode:_
                    ~input_precisions:_
                    (inputs : Function.Input.t) ->
                    List.map inputs ~f:(fun q ->
                      { Function.Data.overflow = Signal.gnd; q }))
              ; propagate_input_overflows = List.map nodes ~f:(Fn.const None)
              }
            ~shared:false
            nodes
        in
        let parent_function_node =
          List.hd_exn alignment_nodes |> Node.parents |> List.hd_exn
        in
        construct_node parent_function_node;
        List.iter alignment_nodes ~f:construct_node;
        alignment_nodes)
  in
  { outputs = aligned_outputs; latency; internal_by_node }
;;

let latency { latency; _ } = latency

let get_node_metadata { internal_by_node; _ } node =
  get_node_metadata ~internal_by_node ~node
;;

module Harden = struct
  module Function_usage_by_cycle = struct
    type t = Function.IO.t list Cycle.Table.t Function.Tag.Table.t
  end

  let assert_data_widths_match_precisions ~context (node : Node.t) data_list =
    let expected_precisions =
      match node with
      | Function { precisions; _ } -> precisions
      | Input { precision; _ }
      | Constant { precision; _ }
      | Function_output_buffer { precision; _ } -> [ precision ]
    in
    if List.length data_list <> List.length expected_precisions
    then
      raise_s
        [%message
          "Number of outputs doesn't match number of precisions"
            (context : string)
            (node : Node.t)
            (List.length data_list : int)
            (List.length expected_precisions : int)];
    List.iter2_exn
      data_list
      expected_precisions
      ~f:(fun (data : Signal.t Function.Data.t) precision ->
        let expected_width = Precision.width precision in
        let actual_width = Signal.width data.q in
        if actual_width <> expected_width
        then
          raise_s
            [%message
              "Signal width doesn't match precision"
                (context : string)
                (node : Node.t)
                (actual_width : int)
                (expected_width : int)
                (precision : Precision.t)];
        let overflow_width = Signal.width data.overflow in
        if overflow_width <> 1
        then
          raise_s
            [%message
              "Overflow signal must be 1 bit wide"
                (context : string)
                (node : Node.t)
                (overflow_width : int)])
  ;;

  (* Construct wires for the outputs of a Function node. Returns one wire per output. *)
  let wire (node : Node.t) =
    match node with
    | Function { precisions; _ } ->
      List.map precisions ~f:(fun p -> Hardcaml.Signal.wire (Precision.width p))
    | Input _ | Constant _ | Function_output_buffer _ ->
      raise_s [%message "Should only be called on Function nodes." (node : Node.t)]
  ;;

  (* Construct and save the output wires for each node in the graph. *)
  let construct_wires scope ~input_signal_by_node t =
    let ( -- ) = Scope.naming scope in
    let input_signal_by_id =
      List.map input_signal_by_node ~f:(fun (node, s) -> Node.id node, s)
      |> Int.Table.of_alist_exn
    in
    Node.postorder_traversal t.outputs ~f:(fun node ->
      let internal = get_node_metadata t node in
      match node with
      | Function_output_buffer { input_function_node; input_function_index; _ } ->
        (* Function_output_buffer directly references the relevant output from its parent
           Function node, which has already been constructed (postorder traversal). *)
        let parent_internal = get_node_metadata t input_function_node in
        internal.output <- [ List.nth_exn parent_internal.output input_function_index ]
      | _ ->
        let qs =
          match node with
          | Input { id; precision; label = _; latency = _ }
          | Constant { id; precision; value = None; label = _ } ->
            (match Hashtbl.find input_signal_by_id id with
             | None ->
               raise_s
                 [%message
                   "[Input]/[Constant] node wasn't specified in [input_signal_by_node]"
                     (node : Node.t)]
             | Some signal ->
               if Signal.width signal <> Precision.width precision
               then
                 raise_s
                   [%message
                     "[construct_wires] width of signal doesn't match precision"
                       (node : Node.t)
                       (Signal.width signal : int)
                       (precision : Precision.t)];
               [ signal ])
          | Constant { precision; value = Some value; label; id = _ } ->
            let width = Precision.width precision in
            if width <= 0
            then
              raise_s
                [%message
                  "Constant width must be positive!"
                    (precision : Precision.t)
                    (value : Bits.t)
                    (label : string)];
            [ Signal.of_bits value ]
          | Function _ -> wire node
          | Function_output_buffer _ ->
            (* Handled above. *)
            assert false
        in
        let overflows =
          match node with
          | Input _ | Constant _ -> List.map qs ~f:(fun _ -> Signal.zero 1)
          | Function _ -> List.map qs ~f:(fun _ -> Signal.wire 1)
          | Function_output_buffer _ -> assert false
        in
        let outputs =
          List.mapi (List.zip_exn qs overflows) ~f:(fun i (q, overflow) ->
            let suffix = if List.length qs > 1 then [%string "%{i#Int}$"] else "" in
            { Function.Data.q = q -- [%string "%{(Node.label node)}$output$%{suffix}q"]
            ; overflow =
                overflow -- [%string "%{(Node.label node)}$output$%{suffix}overflow"]
            })
        in
        assert_data_widths_match_precisions ~context:"construct_wires" node outputs;
        internal.output <- outputs)
  ;;

  let rec pipeline_with_name scope ~name ?(attributes = []) clocking ~n ?enable d =
    let ( -- ) = Scope.naming scope in
    let maybe_add_attributes s = List.fold attributes ~init:s ~f:Signal.add_attribute in
    if n < 0
    then
      raise_s [%message "[pipeline_with_name] cannot accept a negative value" (n : int)]
    else if n = 0
    then d
    else
      maybe_add_attributes
        (Clocking.reg
           clocking
           ?enable
           (pipeline_with_name scope ~name ~attributes clocking ~n:(n - 1) ?enable d))
      -- [%string "%{name}_pipeline_%{n#Int}"]
  ;;

  let pipeline_data_with_name
    scope
    ~name
    ?attributes
    ?enable
    ~n
    clocking
    (t : Signal.t Function.Data.t)
    =
    Function.Data.map2 Function.Data.port_names t ~f:(fun port_name d ->
      pipeline_with_name
        scope
        ~name:(name ^ "$" ^ port_name)
        ?attributes
        clocking
        ~n
        ?enable
        d)
  ;;

  (* Get the single output Data.t for a single-output node. *)
  let get_single_output_exn (node : Node.t) internal =
    match internal.Node_metadata.output with
    | [ output ] -> output
    | other ->
      raise_s
        [%message "Expected single-output node" (node : Node.t) (List.length other : int)]
  ;;

  (* For a given node, properly pipeline all of the inputs and return them. *)
  let construct_node_inputs scope ~clocking ~node t =
    let ( -- ) = Scope.naming scope in
    let node_internal = get_node_metadata t node in
    let parents = Node.parents node in
    let parent_internals = List.map parents ~f:(get_node_metadata t) in
    let parent_outputs =
      match node with
      | Function_output_buffer { input_function_index; _ } ->
        (* The `Function_output_buffer` node is special in the regards that its parent
           (i.e. `Function`) can have multiple outputs. *)
        assert (List.length parent_internals = 1);
        List.map parent_internals ~f:(fun v -> List.nth_exn v.output input_function_index)
      | _ -> List.map2_exn parents parent_internals ~f:get_single_output_exn
    in
    List.mapi
      (List.zip_exn parent_internals parent_outputs)
      ~f:(fun i (parent_internal, parent_output) ->
        match node_internal.endpoints with
        | None ->
          if not (all_parents_are_constant ~internal_by_node:t.internal_by_node ~node)
          then raise_s [%message "All parents need to be Constant!"];
          parent_output
        | Some { start_cycle = node_start; _ } ->
          (* If the parent has timing information, properly pipeline its outputs to the
             inputs of the child. *)
          (match parent_internal.endpoints with
           | Some { end_cycle = parent_end; _ } ->
             pipeline_data_with_name
               scope
               ~name:[%string "%{Node.label node}$input_pipeline$%{i#Int}"]
               ~n:(node_start - parent_end)
               clocking
               parent_output
           | None -> parent_output))
    |> List.mapi ~f:(fun i v ->
      ignore (v.q -- [%string "%{Node.label node}$input$%{i#Int}$q"] : Signal.t);
      ignore
        (v.overflow -- [%string "%{Node.label node}$input$%{i#Int}$node_overflow"]
         : Signal.t);
      v)
    |> fun pipelined_inputs ->
    (* For Function_output_buffer, the single pipelined input should match the node's own
       precision (which is the selected output of the parent Function). For other nodes,
       each pipelined input should match its corresponding parent's precision. *)
    let expected_precisions =
      match node with
      | Function_output_buffer { precision; _ } -> [ precision ]
      | _ -> List.map parents ~f:Node.precision
    in
    List.iter2_exn
      expected_precisions
      pipelined_inputs
      ~f:(fun precision (data : Signal.t Function.Data.t) ->
        let expected_width = Precision.width precision in
        let actual_width = Signal.width data.q in
        if actual_width <> expected_width
        then
          raise_s
            [%message
              "Pipelined input width doesn't match expected precision"
                ~child:(node : Node.t)
                (actual_width : int)
                (expected_width : int)
                (precision : Precision.t)]);
    pipelined_inputs
  ;;

  let wire_up scope ~clocking ~build_mode t =
    let ( -- ) = Scope.naming scope in
    (* Hashtable to hold the result shared operator interfaces. *)
    let time_and_shared_operator_io_by_tag = Function.Tag.Table.create () in
    (* Traversal order doesn't matter here. *)
    Node.postorder_traversal t.outputs ~f:(fun node ->
      let node_internal = get_node_metadata t node in
      (* Construct the properly pipelined inputs to this operator. *)
      let full_inputs = construct_node_inputs scope ~clocking ~node t in
      let outputs = node_internal.output in
      match node with
      | Input _ | Constant _ | Function_output_buffer _ -> ()
      | Function { metadata; shared; _ } ->
        if shared
        then
          (* This is a shared node, so it will be wired up externally to this module,
             based on the sharing scheme. Add it to the table. *)
          Hashtbl.update time_and_shared_operator_io_by_tag metadata.tag ~f:(fun l ->
            let function_io = { Function.IO.i = full_inputs; o = outputs } in
            let new_node =
              (Option.value_exn node_internal.endpoints).start_cycle, function_io
            in
            new_node :: Option.value l ~default:[])
        else (
          (* Create the unshared node, rather than just adding it to the table. *)
          let created_outputs =
            let inputs = List.map full_inputs ~f:(fun { q; overflow = _ } -> q) in
            metadata.create
              (Scope.sub_scope scope (Node.label node))
              ~clocking
              ~build_mode
              ~input_precisions:(List.map (Node.parents node) ~f:Node.precision)
              inputs
          in
          assert_data_widths_match_precisions
            ~context:"wire_up: metadata.create output"
            node
            created_outputs;
          (* Add the input overflows, with the optional overridden overflow propagation. *)
          N_ary.List3.iteri_exn
            outputs
            created_outputs
            metadata.propagate_input_overflows
            ~f:(fun i output { q; overflow } propagate_input_overflows ->
              let overflow =
                let propagate_input_overflows =
                  Option.value
                    propagate_input_overflows
                    ~default:
                      (Function.Propagate_input_overflows.default
                         ~latency:metadata.latency)
                in
                let inputs_overflowed =
                  propagate_input_overflows ~clocking full_inputs
                  |> _ -- [%string "%{Node.label node}$output$%{i#Int}$inputs_overflowed"]
                in
                Signal.( |: ) overflow inputs_overflowed
                -- [%string "%{Node.label node}$output$%{i#Int}$overflow_including_input"]
              in
              Function.Data.Of_signal.assign output { q; overflow })));
    (* Rearrange the operators before returning. *)
    Hashtbl.map time_and_shared_operator_io_by_tag ~f:Int.Table.of_alist_multi
  ;;

  (* This function wires up all of the internal connections in the computation graph.
     Along the way, it collects and returns, for each function type, a mapping of
     start-time to list of I/O at that start time. *)
  let create scope ~clocking ~build_mode ~input_signal_by_node t =
    (* Construct all the initial output wires for the graph. *)
    construct_wires scope ~input_signal_by_node t;
    (* Go through the graph and wire everything up. *)
    wire_up scope ~clocking ~build_mode t
  ;;

  (* Overflow signals flow through the pipeline, so we just collect the overflow signals
     on the output nodes and reduce them together to determine the overall validity of the
     computation. *)
  let get_overflow t =
    (* Fold through the outputs and reduce together all of the overflow signals. *)
    List.filter_map t.outputs ~f:(fun node ->
      let internal = get_node_metadata t node in
      (* Nodes with no endpoints cannot overflow. *)
      Option.map internal.endpoints ~f:(fun _ ->
        (get_single_output_exn node internal).overflow))
    |> Signal.(reduce ~f:( |: ))
  ;;

  let output t =
    List.map t.outputs ~f:(fun node ->
      (get_single_output_exn node (get_node_metadata t node)).q)
  ;;
end
