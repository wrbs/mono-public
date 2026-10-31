open Hardcaml_computation_dsl

(** Create a fully pipelined Hardcaml module implementing a computation dsl graph. *)
module Make
    (Dynamic_inputs : Interface_with_precision.S)
    (Static_inputs : Interface_with_precision.S)
    (Output : Interface_with_precision.S)
    (Graph : sig
       val name : string
       val dynamic_inputs : Node.t Dynamic_inputs.t
       val static_inputs : Node.t Static_inputs.t
       val output : Node.t Output.t
     end) :
  Computation_interface.S
  with module Dynamic_inputs := Dynamic_inputs
   and module Static_inputs := Static_inputs
   and module Output := Output
