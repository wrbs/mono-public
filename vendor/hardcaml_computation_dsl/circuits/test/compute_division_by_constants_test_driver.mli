(** Helper module to run tests on the division with constants module. *)

module Make (Config : sig
    val dividend_signed : bool
    val dividend_bits : int
    val divisors : int list
  end) : sig
  module Compute_division_by_constants :
      module type of
        Hardcaml_computation_dsl_circuits.Compute_division_by_constants.Make (Config)

  val run_model
    :  Bigint.t Compute_division_by_constants.Compute_q1_and_r1.I.t
    -> Bigint.t Compute_division_by_constants.Compute_r3.O.t

  val run_dsl
    :  Bigint.t Compute_division_by_constants.Compute_q1_and_r1.I.t
    -> Bigint.t Compute_division_by_constants.Compute_r3.O.t
end
