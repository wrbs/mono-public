(** Nodes to select from a list of [With_valid.t]s. *)

open Hardcaml
open Hardcaml_computation_dsl

val onehot_select : ?label:string -> Node.t With_valid.t list -> Node.t
val priority_select : ?label:string -> Node.t With_valid.t list -> Node.t With_valid.t

val priority_select_with_default
  :  ?label:string
  -> Node.t With_valid.t list
  -> default:Node.t
  -> Node.t
