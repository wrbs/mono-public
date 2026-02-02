open! Core
open! Hardcaml

type 'a t =
  { fpga_ip : 'a
  ; fpga_port : 'a
  ; host_ip : 'a
  ; host_port : 'a
  }
[@@deriving hardcaml]
