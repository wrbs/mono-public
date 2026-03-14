open! Core
open! Hardcaml_home

val ethertype : Bits.t

module Header : sig
  type 'a t =
    { version : 'a
    ; ihl : 'a
    ; dscp : 'a
    ; ecn : 'a
    ; length : 'a
    ; identification : 'a
    ; flags : 'a
    ; fragment_offset : 'a
    ; ttl : 'a
    ; protocol : 'a
    ; checksum : 'a
    ; src : 'a Addr.Ip.t
    ; dst : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]
end

module Mini_header : sig
  type 'a t =
    { payload_length : 'a
    ; protocol : 'a
    ; src : 'a Addr.Ip.t
    ; dst : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]

  val to_header_without_checksum : Signal.t t -> Signal.t Header.t
end

module Rx : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      ; ethertype : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { header : 'a Mini_header.t
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
      ; header : 'a Header.t
      ; payload : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { tx : 'a Packet_stream.t
      ; payload_ready : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end
