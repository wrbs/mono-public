module Precision = Hardcaml_computation_dsl.Precision

module type S = sig
  (** The number of kernel weights (convolution window size) *)
  val window_size : int

  (** Fixed point precision of the module. *)
  val element_precision : Precision.t
end
