open! Core
open! Hardcaml_home
open! Hardcaml_home_networking

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; rx : 'a Packet_stream.t
    ; rx_valid : 'a
    ; ethertype : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { tx : 'a Packet_stream.t
    ; dst_mac : 'a Addr.Mac.t
    }
  [@@deriving hardcaml]
end

include functor Component.Make_S
