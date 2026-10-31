(** Utilities for working with fixed point numbers. *)

open Core
open Hardcaml
module Precision = Hardcaml_computation_dsl.Precision

(** {2 Conversion functions to Hardcaml Bits} *)

val float_to_fixed_bits : Precision.t -> float# -> Bits.t
val fixed_bits_to_float : Precision.t -> Bits.t -> float#

(** {2 Conversion functions to OCaml integers} *)

val float_to_fixed_int : Precision.t -> float# -> int
val fixed_int_to_float : Precision.t -> int -> float#

(** {2 Quickcheck Generators} *)

(** Generate a normal float value that is representable by the hardware's element
    precision. *)
val generate_float : Precision.t -> float Quickcheck.Generator.t
