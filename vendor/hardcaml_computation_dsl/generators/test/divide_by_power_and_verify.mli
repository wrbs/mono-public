(** A computation to test the
    [Hardcaml_computation_dsl_standard_functions.divide_by_power] and
    [Hardcaml_computation_dsl_standard_functions.verify_divide_by_power] nodes. *)

open Hardcaml_computation_dsl

module Dynamic_inputs : sig
  type 'a t =
    { pow : 'a
    ; dividend : 'a
    }
  [@@deriving hardcaml, compare]

  val precision : Precision.t t
end

module Static_inputs : sig
  include Hardcaml.Interface.Empty

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val precision : Precision.t t
end

module Output : sig
  type 'a t =
    { quotient : 'a
    ; verify_division : 'a
    }
  [@@deriving hardcaml, compare]

  val precision : Precision.t t
end

include
  Tester.To_test
  with module Dynamic_inputs := Dynamic_inputs
   and module Static_inputs := Static_inputs
   and module Output := Output
