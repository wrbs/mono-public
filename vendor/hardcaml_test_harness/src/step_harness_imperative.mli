open Hardcaml
open Hardcaml_step_testbench

module Make (I : Interface.S) (O : Interface.S) : sig
  module Sim : module type of Cyclesim.With_interface (I) (O)

  (*_ We expose the Step module for consistency with the functional version and to improve
    the abstraction here, even though it is not strictly necessary since it isn't
    functorized over I/O *)
  module Step : Imperative.M(Imperative.Cyclesim.Monads).S

  val run
    : (?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> (inputs:Bits.t ref I.t -> outputs:Bits.t ref O.t -> 'a Step.t)
       -> 'a)
        Harness_base.with_test_config

  (** Provides the full cyclesim to the testbench instead of just input and output refs *)
  val run_advanced
    : (?timeout:int
       -> create:(Scope.t -> Signal.t I.t -> Signal.t O.t)
       -> ((Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t -> 'a Step.t)
       -> 'a)
        Harness_base.with_test_config
end
