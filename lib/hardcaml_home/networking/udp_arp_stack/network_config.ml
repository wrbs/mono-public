open! Core
open! Hardcaml

open struct
  module Addr = Hardcaml_home_networking.Addr
end

type 'a t =
  { fpga_mac : 'a Addr.Mac.t
  ; host_ip : 'a Addr.Ip.t
  ; fpga_ip : 'a Addr.Ip.t
  ; host_port : 'a [@bits 16]
  ; fpga_port : 'a [@bits 16]
  }
[@@deriving hardcaml]
