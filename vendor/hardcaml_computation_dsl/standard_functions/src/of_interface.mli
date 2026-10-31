(** Convenience functor to construct [Node]s over an entire [Hardcaml.Interface], rather
    than just over a single signal. *)

open Hardcaml
module Node := Hardcaml_computation_dsl.Node

module Make (X : Hardcaml_computation_dsl.Interface_with_precision.S) : sig
  val mux : ?label:string -> Node.t -> Node.t X.t list -> Node.t X.t
  val mux2 : ?label:string -> Node.t -> Node.t X.t -> Node.t X.t -> Node.t X.t

  val onehot_select
    :  ?label:string
    -> (Node.t, Node.t X.t) With_valid.t2 list
    -> Node.t X.t

  val priority_select
    :  ?label:string
    -> (Node.t, Node.t X.t) With_valid.t2 list
    -> (Node.t, Node.t X.t) With_valid.t2

  val priority_select_with_default
    :  ?label:string
    -> (Node.t, Node.t X.t) With_valid.t2 list
    -> default:Node.t X.t
    -> Node.t X.t

  val zero : Node.t X.t
  val pipeline : label:string -> n:int -> Node.t X.t -> Node.t X.t
end
