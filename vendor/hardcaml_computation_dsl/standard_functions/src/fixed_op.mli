(** Default node constructors for basic fixed-precision mathematical operators. These are
    mostly wrappers of corresponding functions in [Hardcaml_fixed_point]. *)

open Hardcaml
open Hardcaml_computation_dsl
module Node := Hardcaml_computation_dsl.Node

module Op : sig
  type t = Function_tag.Fixed_op.Op.t

  val validate_input_precisions_and_return_output_precision
    :  t
    -> Precision.t list
    -> output_precision:Precision.t * input_signed:bool
end

val add : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val subtract : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val multiply : ?shared:bool -> ?label:string -> Node.t -> Node.t -> Node.t
val div_by_2 : ?label:string -> Node.t -> Node.t
val mux : ?label:string -> Node.t -> Node.t list -> Node.t
val mux2 : ?label:string -> Node.t -> Node.t -> Node.t -> Node.t

val pipeline
  :  ?attributes:Rtl_attribute.t list
  -> label:string
  -> n:int
  -> Node.t
  -> Node.t

val negate : ?label:string -> Node.t -> Node.t
val logical_and : ?label:string -> Node.t -> Node.t -> Node.t
val logical_or : ?label:string -> Node.t -> Node.t -> Node.t
val logical_not : ?label:string -> Node.t -> Node.t
