open! Core
open Hardcaml
module Clocking = Clocking

include struct
  open Static_intf

  module type Arg = Arg
end

module Make (X : Arg) = struct
  type t =
    { latency : int
    ; overflow_cnt : Signal.t
    ; outputs : Signal.t list
    ; output_valid : Signal.t
    ; output_overflow : Signal.t
    ; shared_interfaces : X.shared_interfaces String.Table.t
    }

  let create
    ?(only_clear_counters = false)
    ~build_mode
    scope
    ~(clocking : _ Clocking.t)
    ~start
    ~input_signal_by_node
    ~computation_graph
    =
    let non_counter_clocking =
      if only_clear_counters then { clocking with clear = Signal.gnd } else clocking
    in
    (* Construct the valid tracker - simple shift register with fixed latency. *)
    let latency = Computation_graph.latency computation_graph in
    let valid_pipeline =
      if latency = 0
      then Signal.vdd
      else
        Clocking.reg_fb non_counter_clocking ~width:latency ~f:(fun v ->
          Signal.(sll v ~by:1 |: uresize start ~width:latency))
    in
    let shared_interfaces =
      Computation_graph.Harden.create
        scope
        ~clocking:non_counter_clocking
        ~build_mode
        ~input_signal_by_node
        computation_graph
      |> Hashtbl.mapi ~f:(fun ~key:_metadata ~data ->
        X.process_shared ~valid_pipeline ~interfaces_by_cycle_number:data)
    in
    (* Extract the output signals. *)
    let output_valid = Signal.msb valid_pipeline in
    let output_overflow = Computation_graph.Harden.get_overflow computation_graph in
    { latency
    ; overflow_cnt =
        Clocking.reg_fb
          clocking
          ~enable:(Signal.( &: ) output_valid output_overflow)
          ~width:32
          ~f:(fun v -> Signal.( +:. ) v 1)
    ; outputs = Computation_graph.Harden.output computation_graph
    ; output_valid
    ; output_overflow
    ; shared_interfaces
    }
  ;;
end
