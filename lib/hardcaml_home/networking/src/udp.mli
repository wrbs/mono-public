open! Core
open! Hardcaml_home

val ip_protocol : Bits.t

module Ports : sig
  type 'a t =
    { src : 'a [@bits 16]
    ; dst : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module Rx : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      ; ip_length : 'a [@bits 16]
      ; ip_protocol : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { ports : 'a Ports.t
      ; payload_length : 'a
      ; payload : 'a Packet_stream.t
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end

module Tx : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx_ready : 'a
      ; ports : 'a Ports.t
      ; payload_length : 'a
      ; payload : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { tx : 'a Packet_stream.t
      ; payload_ready : 'a
      ; length : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end
