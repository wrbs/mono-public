(** A single node in the computation DSL. *)

open Hardcaml

type t = private
  | Input of
      { id : int
      ; label : string
      ; precision : Precision.t
      ; latency : int
      (** Inputs can have nonzero latency - this supports shared calculations that take
          place outside of this module. *)
      }
  | Function of
      { id : int
      ; label : string option
      ; metadata : Function.Metadata.t
      (** Relevant information about the function, used in [Computation_graph]
          construction. *)
      ; shared : bool
      ; inputs : t list
      ; precisions : Precision.t list
      }
  (** Supports [Constant] that isn't actually a Hardcaml constant, in case we know
      semantically that it stays valid through the computation. *)
  | Constant of
      { id : int
      ; label : string
      ; precision : Precision.t
      ; value : Bits.t option
      }
  (** The function output buffer is a special node that must follow every function node to
      select a single one of the function node's outputs. The graph construction pass
      verifies and uses this invariant to properly propagate precisions and overflow
      information from the function's list of outputs to downstream nodes. *)
  | Function_output_buffer of
      { id : int
      ; label : string option
      ; input_function_node : t
      ; input_function_index : int
      ; precision : Precision.t
      }
[@@deriving sexp_of]

(** Reset the internal ID counter - used to make debugging the graph outputs easier. *)
val reset_ids : unit -> unit

(** {2 Getters} *)

val id : t -> int
val type_ : t -> string
val label : t -> string
val is_input : t -> bool
val is_constant : t -> bool
val parents : t -> t list
val precision : t -> Precision.t
val latency : t -> int option

(** {2 Constructors} *)

val input : ?latency:int -> label:string -> precision:Precision.t -> unit -> t
val constant : ?value:int -> label:string -> precision:Precision.t -> unit -> t
val constant_bits : ?value:Bits.t -> label:string -> precision:Precision.t -> unit -> t
val unsigned_constant : int -> t

val generic_function
  :  ?label:string
  -> metadata:Function.Metadata.t
  -> shared:bool
  -> t list
  -> t list

(** Like [generic_function], but for functions with exactly one output. Avoids the need to
    wrap/unwrap lists at every call site. *)
val generic_function_single
  :  ?label:string
  -> tag:string
  -> latency:int
  -> infer_output_precision:(Precision.t list -> Precision.t)
  -> create:Function.Create.single
  -> propagate_input_overflows:Function.Propagate_input_overflows.t option
  -> shared:bool
  -> t list
  -> t

(** {2 Traversals} *)

(** Traverse the graph accessible from the given list of nodes, visiting each ancestor
    node exactly once. *)
val preorder_traversal : t list -> f:(t -> unit) -> unit

val postorder_traversal : t list -> f:(t -> unit) -> unit
