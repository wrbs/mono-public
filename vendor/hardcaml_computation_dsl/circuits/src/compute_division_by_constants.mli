(** This module computes the division of an input dividend (with [dividend_bits] bits) by
    a divisor from a pre-specified list. Specifically, it returns q2 and r3 such that
    dividend = divisor * q2 + r3, where r3 ∈ [0, divisor - 1].

    Note that for negative dividends, the q2 result is rounded to negative infinity.

    Coefficients for the required divisor are expected to be pre-computed by SW and looked
    up by HW outside this graph and fed in as an input.

    The computation is split up into several stages to allow for pipelining between
    potential critical paths, and to allow the user to stop just after getting [q2] (if
    they only need the floor division, and not the remainder.

    See the comment at the top of the [.ml] for a detailed explanation. *)

open! Core
open Hardcaml_computation_dsl

module Make (Config : sig
    val dividend_signed : bool
    val dividend_bits : int
    val divisors : int list
  end) : sig
  module Coefficients : sig
    type 'a t =
      { n : 'a
      ; divisor' : 'a
      ; m : 'a
      ; k : 'a
      }
    [@@deriving hardcaml]

    val precision : Precision.t t

    (** This module is useful if the divisor is needed along with the coefficients, i.e.
        if doing any rounding. *)
    module With_divisor : sig
      type nonrec 'a t =
        { divisor : 'a
        ; coefficients : 'a t
        }
      [@@deriving hardcaml]

      val precision : Precision.t t

      include functor Hardcaml.With_valid.Wrap.Include.F
      include functor Hardcaml_axi.Register_bank.Packed_array.Include.F
    end
  end

  val get_coefficients : divisor:int -> int Coefficients.t option

  (** The q2 values are sized based on the max number of bits necessary to represent the
      quotient across all divisors. This value holds the min number of bits necessary for
      q2 across all divisors. *)
  val min_q2_bits : int

  (** This module is just a bit split for a constant divisor, but in the case of multiple
      divisors this requires shifting so a pipeline stage may be necessary. *)
  module Compute_q1_and_r1 : sig
    module I : sig
      type 'a t =
        { dividend : 'a
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      val precision : Precision.t t
    end

    module O : sig
      type 'a t =
        { unsigned_q1 : 'a
        ; q1 : 'a
        ; r1 : 'a
        ; dividend_msb : 'a option (** Some if using a signed dividend. *)
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      val precision : Precision.t t
    end

    val model : Bigint.t I.t -> Bigint.t O.t
    val dsl : Node.t I.t -> Node.t O.t
  end

  (** This module involves an [dividend_bits] by [dividend_bits] multiplication (at
      worst), followed by a trunction to only the top dividend_bits bits. Consider
      pipelining after this step if your inputs are very wide. *)
  module Compute_q2 : sig
    module I = Compute_q1_and_r1.O

    module O : sig
      type 'a t =
        { q1 : 'a
        ; r1 : 'a
        ; q2 : 'a
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      val precision : Precision.t t
    end

    val model : Bigint.t I.t -> Bigint.t O.t
    val dsl : division_multiply_cycles:int -> Node.t I.t -> Node.t O.t
  end

  (** This module involves an [dividend_bits] by [dividend_bits] multiplication and a
      subtraction (to compute a remainder) and some bit concatenations. Consider
      pipelining after this step if your inputs are wide. *)
  module Compute_r3 : sig
    module I = Compute_q2.O

    module O : sig
      type 'a t =
        { q2 : 'a
        ; r3 : 'a
        }
      [@@deriving hardcaml]

      val precision : Precision.t t
    end

    val model : Bigint.t I.t -> Bigint.t O.t

    val dsl
      :  remainder_multiply_cycles:int
      -> remainder_subtraction_cycles:int
      -> Node.t I.t
      -> Node.t O.t
  end

  (** Chain together all the component pieces. *)
  val model : Bigint.t Compute_q1_and_r1.I.t -> Bigint.t Compute_r3.O.t

  val dsl
    :  division_multiply_cycles:int
    -> remainder_multiply_cycles:int
    -> remainder_subtraction_cycles:int
    -> Node.t Compute_q1_and_r1.I.t
    -> Node.t Compute_r3.O.t

  module For_testing : sig
    val expect : dividend:Bigint.t -> divisor:Bigint.t -> Bigint.t Compute_r3.O.t
  end
end
