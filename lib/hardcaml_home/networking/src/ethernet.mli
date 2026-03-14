open! Core
open! Hardcaml_home

module Header : sig
  type 'a t =
    { dst : 'a Addr.Mac.t
    ; src : 'a Addr.Mac.t
    ; ethertype : 'a
    }
  [@@deriving hardcaml]
end

module Rx : sig
  module I : sig
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    (* header outputs valid on payload start, until the next cycle after payload stop *)
    type 'a t =
      { header : 'a Header.t
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
      ; last_header_byte : 'a
      }
    [@@deriving hardcaml]
  end

  include functor Component.Make_S
end
