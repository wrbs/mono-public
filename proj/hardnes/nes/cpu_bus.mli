open! Core
open! Hardcaml
open! Signal
(*
   (** Models the NES cpu bus, trying to deal with complexities like:

    - open bus behavior
    - bus conflicts

    The main reason it exists is the nes just has one bus for reads/writes and stuff,
    whereas the

    relevant times:

    CLAUDE HELP ME

    m2 falling:

    - cpu samples + reads from current data
    - we actually clock the cpu
    - mappers should clock too

    m2 rising cycle (m2 low):

    - values are 'stable', if ppu timing matters _this_ is when you should perform the
      io/start the read

    one master clock after m2 falls:

    - addr/cpu write data/... is _actually_ stable one master clock cycle after m2 falls
      rather than when m2 rises. so if reads take a while you actually have like a bunch
      of master clocks to perform them, just need that the data/mask *)

module For_cpu : sig
  module I : sig
    type 'a t = { read_data : 'a } [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { addr : 'a
      ; write_data : 'a
      ; rw : 'a
      }
    [@@deriving hardcaml]
  end
end

module For_component : sig
  module I : sig
    type 'a t =
      { addr : 'a
      ; write_data : 'a
      ; rw : 'a
      ; m2 : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    (** The output: can be a combinational function of addr/write_data/rw/... *)

    type 'a t =
      { read_data : 'a (* which bits the *)
      ; read_mask : 'a (* which bits the thing is driving *)
      }
    [@@deriving hardcaml]
  end
end

type t

(** Main loop *)

val create : Scope.t -> clock:Signal.t -> clear:Signal.t -> t
val complete : t -> from_cpu:Signal.t For_cpu.O.t -> unit

(** Inputs *)

val cpu_in : t -> Signal.t For_cpu.I.t
val component_in : t -> Signal.t For_component.I.t

(** Adding a bus writer *)

val add : t -> Signal.t For_component.O.t -> unit *)
