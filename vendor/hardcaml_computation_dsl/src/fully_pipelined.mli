(** Generate a fully pipelined computation pipeline based on the given computation graph.
    This module does not share any resources and provides maximum throughput. *)

open Hardcaml

type t =
  { latency : int
  ; overflow_cnt : Signal.t
  ; outputs : Signal.t list
  ; output_valid : Signal.t
  ; output_overflow : Signal.t
  }

val create
  :  ?only_clear_counters:bool (** See [static_intf.ml] for details. *)
  -> build_mode:Build_mode.t
  -> Scope.t
  -> clocking:Signal.t Clocking.t
  -> start:Signal.t
  -> input_signal_by_node:(Node.t * Signal.t) list
  -> computation_graph:Computation_graph.t
  -> t
