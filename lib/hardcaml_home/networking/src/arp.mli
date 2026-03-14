open! Core
open! Hardcaml_home

val ethertype : Bits.t

module Oper : sig
  val request : Bits.t
  val reply : Bits.t
end

module Fields : sig
  type 'a t =
    { oper : 'a [@bits 16]
    ; sha : 'a Addr.Mac.t
    ; spa : 'a Addr.Ip.t
    ; tha : 'a Addr.Mac.t
    ; tpa : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]
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
      { fields : 'a Fields.t
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
      ; fields : 'a Fields.t
      ; start : 'a
      ; abort : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { tx : 'a Packet_stream.t
      ; ready_to_start : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end
