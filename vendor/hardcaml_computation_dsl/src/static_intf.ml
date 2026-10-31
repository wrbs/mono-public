(** Generate a statically scheduled computation pipeline based on the given computation
    graph. Doesn't accept pushback - assumes that results are drained into a FIFO on the
    output. This can be used to construct various types of static backends - specifically,
    it is used for a fully-pipelined backend and a shared operator backend. *)

open Core
open Hardcaml

module type Arg = sig
  type shared_interfaces

  (** Function that dictates how to reduce nodes that share a computation resource to a
      single bus. *)
  val process_shared
    :  valid_pipeline:Hardcaml.Signal.t
    -> interfaces_by_cycle_number:Function.IO.t list Int.Table.t
    -> shared_interfaces
end

module type Static = sig
  module type Arg = Arg

  module Make (X : Arg) : sig
    type t =
      { latency : int
      ; overflow_cnt : Signal.t
      ; outputs : Signal.t list
      ; output_valid : Signal.t
      ; output_overflow : Signal.t
      ; shared_interfaces : X.shared_interfaces String.Table.t
      }

    val create
      :  ?only_clear_counters:bool
           (** Only use a clear signal on the counters. Don't propagate clears through any
               of the computation pipeline. This should generally be safe for
               fully-pipelined computation modules with a reasonable reset discipline
               (hold reset for many cycles and wait before starting inputs), but we
               default to [false] so nobody has any unexpected behavior. *)
      -> build_mode:Build_mode.t
      -> Scope.t
      -> clocking:Signal.t Clocking.t
      -> start:Signal.t
      -> input_signal_by_node:(Node.t * Signal.t) list
      -> computation_graph:Computation_graph.t
      -> t
  end
end
