open! Core
open! Hardcaml

module Include = struct
  module Waveform = Hardcaml_waveterm.Waveform
end

open Include

module type Component = sig
  module I : Interface.S
  module O : Interface.S

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module type Sim_S = sig
  module I : Interface.S
  module O : Interface.S

  module Sim : module type of struct
    include Cyclesim.With_interface (I) (O)
  end

  val create : ?config:Cyclesim.Config.t -> unit -> Sim.t
  val create_waveform : unit -> Waveform.t * Sim.t
  val inputs : Sim.t -> Bits.t ref I.t
  val outputs : Sim.t -> edge:Side.t -> Bits.t ref O.t
  val io : Sim.t -> edge:Side.t -> Bits.t ref I.t * Bits.t ref O.t
  val update_inputs : Sim.t -> (Bits.t I.t -> Bits.t I.t) -> unit

  (** Main helper *)
  val testbench : ?vcd:string -> ?waveform:(Waveform.t -> unit) -> (Sim.t -> unit) -> unit
end

module type Hardcaml_home_test_helpers = sig
  include module type of struct
    include Include
  end

  module type Component = Component

  module Make_sim (C : Component) : Sim_S with module I = C.I and module O = C.O

  val hex_bits : string -> Bits.t
end
