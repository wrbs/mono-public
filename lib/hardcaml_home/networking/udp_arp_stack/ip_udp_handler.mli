open! Core
open! Hardcaml_home
open! Hardcaml_home_networking

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; header_in : 'a Ethernet.Header.t
    ; rx : 'a Packet_stream.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; udp_out : 'a Udp_interface.Out.t
    ; rx_valid : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { tx : 'a Packet_stream.t
    ; header_out : 'a Ethernet.Header.t
    ; udp_in : 'a Udp_interface.In.t
    }
  [@@deriving hardcaml]
end

include functor Component.Make_S
