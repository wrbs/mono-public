open! Core
open! Hardcaml
open! Signal

open struct
  open Hardcaml_hobby_boards
  module Axi32 = Ethernet.Axi32
  module Ethernet = Ethernet
  module Ethernet_types = Ethernet_types
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; rx : 'a Axi32.Source.t
    ; downstream_tx : 'a Axi32.Source.t
    ; tx_ready : 'a Axi32.Dest.t
    ; conf : 'a Network_conf.t
    }
end

module O = struct
  type 'a t =
    { tx : 'a Axi32.Source.t
    ; downstream_tx_ready : 'a Axi32.Dest.t
    ; downstream_rx : 'a Axi32.Source.t
    ; host_mac : 'a Network_conf.t
    ; host_mac_valid : 'a
    }
end

module Arp = struct
  module Request = struct
    type 'a t =
      { oper : 'a
      ; sha : 'a [@bits 48]
      ; spa : 'a [@bits 32]
      ; tha : 'a [@bits 48]
      ; tpa : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  module Packet = struct
    type 'a t =
      { htype : 'a [@bits 16]
      ; ptype : 'a [@bits 16]
      ; hlen : 'a [@bits 8]
      ; plen : 'a [@bits 8]
      ; request : 'a Request.t
      }
    [@@deriving hardcaml]
  end
end

module State = struct
  type t =
    | Idle
    | Ethernet_header
    | Arp
  [@@deriving sexp_of, enumerate, compare ~localize]
end
