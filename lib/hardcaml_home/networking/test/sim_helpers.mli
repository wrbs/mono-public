open! Core
open! Hardcaml
open! Hardcaml_home_networking

include module type of struct
  include Hardcaml_home_test_helpers
end

val sfd : Bits.t
val with_sfd : Bits.t -> Bits.t

val skip_to_stream_start
  :  ?timeout:int
  -> ('a, 'b) Cyclesim.t
  -> stream_before_cycle:Bits.t ref Packet_stream.t
  -> update_inputs:(('a, 'b) Cyclesim.t -> unit)
  -> unit

val consume_stream_result
  :  ?timeout:int
  -> ('a, 'b) Cyclesim.t
  -> stream_before_cycle:Bits.t ref Packet_stream.t
  -> valid_before_cycle:Bits.t ref
  -> update_inputs:(('a, 'b) Cyclesim.t -> unit)
  -> string * [> `Abort | `Stop ]

val consume_stream
  :  ?timeout:int
  -> ('a, 'b) Cyclesim.t
  -> stream_before_cycle:Bits.t ref Packet_stream.t
  -> valid_before_cycle:Bits.t ref
  -> update_inputs:(('a, 'b) Cyclesim.t -> unit)
  -> string

module Queue_driver : sig
  type t

  val create : unit -> t
  val add : t -> (unit -> unit) -> unit
  val wait : t -> int -> unit
  val add_step : t -> (unit -> unit) -> reset:(unit -> unit) -> unit
  val update_inputs : t -> unit
end

module Ethernet_driver : sig
  val gap : int

  type t

  val create : Bits.t ref Rmii.Hw.Rx.t -> t
  val add_packet : t -> packet:Bits.t -> unit
  val update_inputs : t -> unit
end
