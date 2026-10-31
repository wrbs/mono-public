(** A general tester module for any Hardcaml computation module, against a given OCaml
    model. Generates quickcheck scenarios for testing. *)

open Core

(** The computation module to be tested, along with a OCaml model that represents the
    desired function. *)
module type To_test = sig
  include Hardcaml_computation_dsl_generators.Computation_interface.S

  (** The model outputs an option for each field (including [overflow]) - if the value is
      [None], this indicates that the test framework does not need to check this value.

      The handwritten models can use this to encode the importance of checks - i.e.
      usually, if [overflow] is [true], then the rest of the values can be [None], because
      we don't care what they are. *)
  val model
    :  dynamic_inputs:float Dynamic_inputs.t
    -> static_inputs:float Static_inputs.t
    -> q:float option Output.t * overflow:bool option
end

module type S = sig
  module To_test : To_test

  (** Quickcheck generators for the dynamic and static inputs. *)
  module Quickcheck_generators : sig
    val dynamic_inputs : float To_test.Dynamic_inputs.t Quickcheck.Generator.t
    val static_inputs : float To_test.Static_inputs.t Quickcheck.Generator.t
  end

  (** Run a single test, where the static inputs are set once, several sets of dynamic
      inputs are sent one after another, and the outputs are checked against the given
      model. *)
  val run_test
    :  ?verbose:bool
         (** Whether or not to verbosely print output while validating. Default [false]. *)
    -> ?waves:bool (** Whether or not to dump the waveform to file. Default [false]. *)
    -> static_inputs:float To_test.Static_inputs.t
    -> dynamic_inputs_list:float To_test.Dynamic_inputs.t list
    -> unit
    -> unit
end

module type Tester = sig
  module type To_test = To_test
  module type S = S

  module Make (To_test : To_test) : S with module To_test := To_test
end
