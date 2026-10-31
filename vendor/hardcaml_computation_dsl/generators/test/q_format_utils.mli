(** Utility file that implements the q-format for testing. *)

open Core
module Precision := Hardcaml_computation_dsl.Precision

val generate : Precision.t -> float Quickcheck.Generator.t

val round_to_precision
  :  ?rounding:[ `Truncate | `Round_nearest ]
  -> precision:Precision.t
  -> float
  -> q:float * overflow:bool
