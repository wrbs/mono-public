module Precision = Hardcaml_computation_dsl.Precision

module type S = sig
  (** Size of the convolution window. *)
  val window_size : int

  (** Fixed point precision of the module. *)
  val element_precision : Precision.t
end
