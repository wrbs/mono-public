(** This module implements division by a fixed range of powers of a given base value,
    assuming that the input is an integer multiple of the value it is being divided by.

    Specifically, we statically configure [base] and the range of powers of [base] (given
    by [pow_base_bounds]) that this module can divide by. Then, it will exactly produce
    [dividend / base^pow] as the output. For now, it only supports odd values for [base].

    The implementation is based on methods described in:

    "Division by Invariant Integers using Multiplication" by Granlund and Montgomery.
    (https://gmplib.org/~tege/divcnst-pldi94.pdf)

    We also include an optional mode that checks whether the dividend was actually an
    integer multiple of the divisor. This causes the internal multiply to use more bits. *)
open Hardcaml

(** A module to represent inclusive ranges of integers. Specifically, it is used in this
    context to represent bounds of exponents. *)
module Inclusive_integer_range : sig
  include module type of
      Hardcaml_circuits.Exactly_divisible_by_constant_power.Inclusive_integer_range

  (** Negate a given range. *)
  val ( ~- ) : t -> t

  (** Get the range of values that could result from adding values from the given ranges. *)
  val ( + ) : t -> t -> t

  (** Get the range of values that could result from subtracting values from the given
      ranges. *)
  val ( - ) : t -> t -> t

  (** The precision of a value that lies in this range. *)
  val precision : t -> Hardcaml_computation_dsl.Precision.t

  (** Returns whether or not the given integer lies within the range. *)
  val within : t -> int -> bool
end

module type Config = sig
  (** The base of the power that is going to serve as the divisor in the division. *)
  val base : int

  (** The range of powers of [base] that this module supports dividing by. *)
  val pow_base_bounds : Inclusive_integer_range.t

  (** The number of bits in the input integer. *)
  val input_i_bits : int

  (** The number of bits in the output integer. *)
  val output_i_bits : int

  (** The latency of the ROM lookup in the calculation. *)
  val rom_latency : int

  (** The latency of the multiply in the calculation. *)
  val multiply_latency : int

  (** Whether or not this module should internally drive [error] when performing a
      division (i.e. [pow] is positive). This performs two checks -

      (1) Whether the output of the division will fit in [output_i_bits].
      (2) Whether the [dividend] is an integer multiple of [base ** pow].

      Setting to [true] makes the internal multiply wider and adds some extra logic. If
      not setting to true, you must instantiate a secondary [Divide_by_power] node to
      verify the multiplication. This is advanced usage and only recommended if the extra
      multiply width is truly detrimental to performance. *)
  val check_for_error : bool

  val signedness : Signedness.t
end

module Make (Config : Config) : sig
  (** The end-to-end latency of this module, fully-pipelined. *)
  val latency : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; dividend : 'a
      ; pow : 'a
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { quotient : 'a
      ; error : 'a
      (** This output is only valid if [pow > 0] or the [Config.check_for_error] flag is
          set when instantiating the module. It is valid with the [valid] pulse. It
          indicates two error checks:

          (1) Whether the input was too large - i.e. dividing by the given power would
              result in a value that doesn't fit within [Config.output_i_bits].

          (2) Whether [dividend] was in fact an integer multiple of [Config.base ** pow]. *)
      ; division_output_not_multiple : 'a
      (** This output indicates the (2) case above, and is high only when the input is not
          an integer multiple of the divisor. *)
      ; valid : 'a (** Pipelined version of the input [valid], used to track dataflow. *)
      }
    [@@deriving hardcaml]
  end

  val hierarchical
    :  ?instance:string
    -> Scope.t
    -> build_mode:Build_mode.t
    -> Interface.Create_fn(I)(O).t
end
