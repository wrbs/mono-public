open! Core
open! Hardcaml_home
open! Hardcaml_home_networking

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; hw_rx : 'a Rmii.Hw.Rx.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; udp_out : 'a Udp_interface.Out.t
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { hw_tx : 'a Rmii.Hw.Tx.t
    ; udp_in : 'a Udp_interface.In.t
    }
  [@@deriving hardcaml]
end

include functor Component.Make_S
