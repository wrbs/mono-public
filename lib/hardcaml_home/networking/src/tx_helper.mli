open! Core
open! Hardcaml

module Make (Header : Interface.S) : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx_ready : 'a
      ; header : 'a Header.t
      ; payload : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { tx : 'a Packet_stream.t
      ; payload_ready : 'a
      ; last_header_byte : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end
