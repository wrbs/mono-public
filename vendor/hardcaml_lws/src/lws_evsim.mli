open Hardcaml
module Evsim = Hardcaml_event_driven_sim.Two_state_simulator

include Lws_runtime.S (** @inline *)

module Backend_specific_config : sig
  module Clock : sig
    type t =
      { port_name : string
      ; time : int
      ; initial_delay : int
      }
  end

  module Common : sig
    type t =
      { clocks : Clock.t list
      ; default_clock_name : string option
      (** When specified, all tasks by default will be spawned in the above clock domain,
          if ~trigger is not specified. *)
      ; waves : bool
      }
  end

  type t =
    { common : Common.t
    ; backend : Evsim.Config.t
    }
end

module With_interface (I : Hardcaml.Interface.S) (O : Interface.S) : sig
  type sim_context = (Bits.t ref I.t, Bits.t ref O.t) Lws_context.t
  type create_fn = Scope.t -> Signal.t I.t -> Signal.t O.t

  val create
    :  ?config:Lws.Config.t
    -> backend_specific_config:Backend_specific_config.t
    -> create_fn
    -> sim_context t
end

val to_polymorphic : 'context t -> 'context Lws_runtime.t
