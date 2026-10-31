(** Implementation of a resizing function to change the precisions of nodes in the graph. *)

module Node := Hardcaml_computation_dsl.Node
module Precision := Hardcaml_computation_dsl.Precision
open Function_tag.Resize

type t =
  ?label:string
  -> ?round:Round.t
       (** How to round this value. Default is set by [Fixed_signed.resize_with_overflow]. *)
  -> ?overflow:Overflow.t
       (** How to deal with overflow for this value. Default is set by
           [Fixed_signed.resize_with_overflow]. *)
  -> precision:Precision.t
  -> Node.t
  -> Node.t

val lossless : t
val safe : t
val unsafe : t
