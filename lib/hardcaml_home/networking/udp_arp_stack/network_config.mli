open! Core
open! Hardcaml
module Addr := Hardcaml_home_networking.Addr

type 'a t =
  { fpga_mac : 'a Addr.Mac.t
  ; host_ip : 'a Addr.Ip.t
  ; fpga_ip : 'a Addr.Ip.t
  ; host_port : 'a
  ; fpga_port : 'a
  }
[@@deriving hardcaml]
