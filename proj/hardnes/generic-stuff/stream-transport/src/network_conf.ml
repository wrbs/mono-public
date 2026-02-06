open! Core
open! Hardcaml

let ip_bits = 32
let port_bits = 16

type 'a t =
  { fpga_ip : 'a [@bits ip_bits]
  ; fpga_port : 'a [@bits port_bits]
  ; host_ip : 'a [@bits ip_bits]
  ; host_port : 'a [@bits port_bits]
  }
[@@deriving hardcaml]
