(** A cyclesim-backed backend for running LWS programs.

    When using the cyclesim backend, there are two options user can opt to work with:
    1. Using Lws_cyclesim.With_interface(I)(O).run . This is useful for standalone
       testbenches that only needs to interact with hardware simulation component. This is
       the vast majority of testbeds.
    2. Using the Lws_backend.S set of APIs, which gives a procedural API to interact with
       the simulator. This is useful when the LWS simulation is running as part of a
       bigger test suite with its own harness / setup / teardown infrastructure. *)

open Hardcaml

include Lws_runtime.S (** @inline *)

(** Steps the simulator by [n] cycles, which will result in all scheduled tasks to
    progress by one cycle. *)
val cycle : ?n:int -> _ t -> unit

(** Returns how many times [cycle] has been called. *)
val cycle_num : _ t -> int

val resolve_trigger_by_period : _ t -> int -> Lws.Trigger.t option

(** Controls waveform tracing. [`on] replaces the current [Waveform.t] with a freshly
    traced one. [`off] disables tracing, causing
    [Lws_context.Simulator_specific.Cyclesim.waveform] to return [None]. *)
val waveform_trace : _ Lws_context.t t -> [ `on | `off ] -> unit

(** Returns the assertion tracking state. It is only populated when the simulator was
    created with [Lws.Config.assertions = true]; otherwise [Assertions.results] returns
    [None]. See [Hardcaml.Assertions]. *)
val assertions : _ t -> Assertions.t

module Backend_specific_config : sig
  module Simulation_backend = Hardcaml_verilator.Simulation_backend

  type t =
    { cyclesim_config : Cyclesim.Config.t
    ; simulation_backend : Simulation_backend.t
    ; waves : bool
    }

  val default : t
  val default_with_clock_mode : Cyclesim.Config.Clock_mode.t -> t
end

module With_interface (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) : sig
  type sim_context = (Bits.t ref I.t, Bits.t ref O.t) Lws_context.t
  type create_fn = Scope.t -> Signal.t I.t -> Signal.t O.t

  val create
    :  ?config:Lws.Config.t
    -> ?backend_specific_config:Backend_specific_config.t
    -> create_fn
    -> sim_context t
end

val to_polymorphic : 'context t -> 'context Lws_runtime.t
