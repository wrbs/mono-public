(** Represent a function in the computation DSL. This is the main type that is used to
    represent various computations in the graph. *)

open Core

(** The data that moves through this DAG is a Hardcaml signal and an overflow flag.

    This type is similar for [Fifo.t] - the widths are unset, so we do not use
    hierarchical circuits here. *)
module Data = struct
  type 'a t =
    { q : 'a
    ; overflow : 'a
    }
  [@@deriving hardcaml]

  include functor Hardcaml.With_valid.Wrap.Include.Make
end

(** The input to a function is just a list of Hardcaml signals. The precisions of these
    signals are inferred and passed separately; this is a general interface that allows
    any function to fit in this framework. *)
module Input = struct
  type t = Hardcaml.Signal.t list

  (** In particular cases where we want full control, we need to know the full input,
      including the overflow flags. In those situations, use [Input.Full.t], instead of
      [Input.t]. *)
  module Full = struct
    type t = Hardcaml.Signal.t Data.t list
  end
end

(** A function can have multiple outputs. *)
module Output = struct
  type t = Hardcaml.Signal.t Data.t list
end

(** This is the create type of an implementation of a [Function] - users of the DSL can
    write arbitrary functions that expose a [Create.t] and plug into this framework. *)
module Create = struct
  type t =
    Hardcaml.Scope.t
    -> clocking:Hardcaml.Signal.t Hardcaml.Clocking.t
    -> build_mode:Hardcaml.Build_mode.t
    -> input_precisions:Precision.t list
    -> Input.t
    -> Output.t
  [@@deriving sexp_of]

  type single =
    Hardcaml.Scope.t
    -> clocking:Hardcaml.Signal.t Hardcaml.Clocking.t
    -> build_mode:Hardcaml.Build_mode.t
    -> input_precisions:Precision.t list
    -> Input.t
    -> Hardcaml.Signal.t Data.t
  [@@deriving sexp_of]
end

(** This is the type of the [propagate_input_overflows] function in a [Function] - users
    of the DSL can write arbitrary functions that expose a function of this type to
    override the default behavior (of just pipelining and OR'ing together all of the input
    overflow signals). This default works for most functions (except [mux] or some other
    function that has conditional branches on the input). *)
module Propagate_input_overflows = struct
  type t =
    clocking:Hardcaml.Signal.t Hardcaml.Clocking.t
    -> Input.Full.t
       (** The full list of inputs to this function, used to infer the appropriate
           overflow propagation. *)
    -> Hardcaml.Signal.t
  [@@deriving sexp_of]

  let default ~latency ~clocking (inputs : Input.Full.t) =
    List.map inputs ~f:(fun v -> v.overflow)
    |> Hardcaml.Signal.(reduce ~f:( |: ))
    |> Hardcaml.Clocking.pipeline clocking ~n:latency
  ;;
end

(** Per-output overflow propagation overrides. Each element corresponds to one output of
    the function. *)
module Propagate_input_overflows_multi = struct
  type t = Propagate_input_overflows.t option list [@@deriving sexp_of]
end

module Tag = String

(** Defines all of the function-associated information that the DSL needs to construct the
    computation graph, infer widths, and harden the function. *)
module Metadata = struct
  type t =
    { tag : Tag.t (** Unique identifier for the function. *)
    ; latency : int (** Latency of the function. *)
    ; infer_output_precision : Precision.t list -> Precision.t list
    (** Infer the output precisions of the function, given all of the input precisions.
        Returns one precision per output. *)
    ; create : Create.t (** Create function. *)
    ; propagate_input_overflows : Propagate_input_overflows_multi.t
    (** Per-output overflow propagation overrides. Must have the same length as the number
        of outputs. Each element can optionally override the default behavior (of just
        pipelining and OR'ing together all of the input overflow signals). *)
    }
  [@@deriving sexp_of]

  let to_string { latency; _ } = [%string "{latency = %{latency#Int}}"]
end

(** This type is used to represent the I/O interface to a function. It is used when
    representing shared functions (in this case, we have to pass around the I/O interface
    to be assigned at a different level from graph construction. *)
module IO = struct
  type t =
    { i : Input.Full.t
    ; o : Output.t
    }
end

module type S = sig
  val create : Create.t
  val tag : string
end

module type S_arg = sig
  module type Arg

  module Make (Arg : Arg) : sig
    val create : Create.t
  end

  val tag : string
end

module type Function = sig
  module Data = Data
  module Tag = Tag
  module Metadata = Metadata
  module Input = Input
  module Output = Output
  module IO = IO
  module Create = Create
  module Propagate_input_overflows = Propagate_input_overflows
  module Propagate_input_overflows_multi = Propagate_input_overflows_multi

  module type S_arg = S_arg
  module type S = S
end
