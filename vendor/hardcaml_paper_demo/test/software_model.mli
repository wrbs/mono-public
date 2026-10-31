(** Software model of the 1D convolution with fixed-point arithmetic modeling. *)

module Precision = Hardcaml_computation_dsl.Precision

(** Compute 1D convolution of [input] with [kernel], modeling the fixed-point arithmetic
    including quantization and overflow. Returns one output per input element. *)
val conv1d : Precision.t -> kernel:float list -> input:float list -> float list
