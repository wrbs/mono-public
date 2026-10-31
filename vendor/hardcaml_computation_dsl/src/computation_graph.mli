(** Given a DAG of [Node.t]s, this module constructs a full graph representation,
    including width inference. It also includes a [Harden] module, which lowers the
    representation from a DAG to actual Hardcaml. *)

open Core
open Hardcaml
module Clocking = Clocking

type t

(** Construct a graph from a list of [Node] descriptions - doesn't wire anything up, just
    does static analysis over the given graph. *)
val of_output_nodes : Node.t list -> t

(** Return the latency of this entire operation. *)
val latency : t -> int

(** Rename just for more descriptive types. *)
module Cycle = Int

(** Module to construct and wire up real signals, given input signals. *)
module Harden : sig
  (** A type to represent the data structure we use to pass around shared operators.
      Specifically, it is a 2D table keyed by function tag and cycle number where the
      value is the list of nodes (represented by input bus and output wire) that want to
      use that function on the given clock cycle. *)
  module Function_usage_by_cycle : sig
    type t = Function.IO.t list Cycle.Table.t Function.Tag.Table.t
  end

  val create
    :  Scope.t
    -> clocking:Signal.t Clocking.t
    -> build_mode:Build_mode.t
    -> input_signal_by_node:(Node.t * Signal.t) list
    -> t
    -> Function_usage_by_cycle.t

  val get_overflow : t -> Signal.t
  val output : t -> Signal.t list
end

(** The start and end cycles of each function in the static computation. Stored (below)
    for each node. *)
module Start_and_end_cycles : sig
  type t =
    { start_cycle : Cycle.t
    ; end_cycle : Cycle.t
    }
  [@@deriving sexp_of, fields ~getters]

  module Option : sig
    type nonrec t = t option

    val to_string : t -> string
  end
end

(** Internal data stored per [Node]; once the [Computation_graph] is constructed,
    [get_node] can be used to query this internal state. This shouldn't be used by end
    users - only by the src/ layer that wraps this kernel. *)
module Node_metadata : sig
  type t = private
    { endpoints : Start_and_end_cycles.t option
    ; mutable output : Signal.t Function.Data.t list
    (** Empty when the graph is initially constructed; populated during the [wire_up]
        phase. For [Function] nodes, this list has one element per output. For all other
        node types, it has exactly one element. *)
    }
  [@@deriving fields ~getters]
end

val get_node_metadata : t -> Node.t -> Node_metadata.t
