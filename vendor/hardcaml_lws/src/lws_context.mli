(** Ports the testbench can interact with in simulations. *)

open Core
open Hardcaml

module Simulator_specific : sig
  module Cyclesim : sig
    type ('i, 'o) t

    val simulator : ('i, 'o) t -> ('i, 'o) Cyclesim.t
  end

  module Evsim : sig
    module Evsim = Hardcaml_event_driven_sim.Two_state_simulator

    type t

    val simulator : t -> Evsim.Simulator.t
  end

  type ('i, 'o) t =
    | Cyclesim of ('i, 'o) Cyclesim.t
    | Evsim of Evsim.t
    | Cosim
end

type ('i, 'o) internal

type ('i, 'o) t =
  { inputs : 'i
  ; outputs : 'o Before_and_after_edge.t
  ; internal : ('i, 'o) internal
  }

val waveform : ('i, 'o) t -> Hardcaml_waveterm.Waveform.t option

module Waveform : sig
  type 'a with_options = 'a Hardcaml_waveterm_kernel.Waveform.with_options

  val print : ?channel:Out_channel.t -> (('i, 'o) t -> unit) with_options

  val expect
    :  ?show_digest:bool
    -> ?serialize_to:string
    -> (('i, 'o) t -> unit) with_options

  val expect_exact
    :  ?show_digest:bool
    -> ?serialize_to:string
    -> (('i, 'o) t -> unit) with_options
end

(** Access the underlying simulator object. Users should be careful interacting with the
    underlying simulator, as that can mess with how LWS interacts with the simulators. *)
val simulator_specific : ('i, 'o) t -> ('i, 'o) Simulator_specific.t

val resolve_trigger_by_clock_port_name : _ t -> string -> Lws_trigger.t option

(** Similar to [Lws.spawn], but it implicitly looks up the appropriate trigger for running
    a computation in the clock domain belonging to the clock specified. Raises if the
    clock is undefined. *)
val spawn_in_clock_domain
  :  here:[%call_pos]
  -> Lws.Handler.t @ local
  -> _ t
  -> clock_domain:[ `Port_name of string ]
  -> (Lws.Handler.t @ local -> 'a)
  -> 'a Lws.Task.t

module M (I : T1) (O : T1) : sig
  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t
end

module Private : sig
  val create
    :  inputs:'i
    -> outputs:'o Before_and_after_edge.t
    -> waveform:Hardcaml_waveterm.Waveform.t option
    -> simulator_specific:('i, 'o) Simulator_specific.t
    -> trigger_by_clock_port_name:Lws_trigger.t String.Map.t
    -> ('i, 'o) t

  module Simulator_specific : sig
    module Cyclesim : sig
      val create
        :  simulator:('i, 'o) Hardcaml.Cyclesim.t
        -> ('i, 'o) Simulator_specific.Cyclesim.t

      val set_simulator_and_waveform
        :  ('i, 'o) t
        -> ('i, 'o) Hardcaml.Cyclesim.t
        -> Hardcaml_waveterm.Waveform.t option
        -> unit
    end

    module Evsim : sig
      val create
        :  simulator:Simulator_specific.Evsim.Evsim.Simulator.t
        -> Simulator_specific.Evsim.t
    end
  end
end
