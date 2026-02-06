open! Core
open! Hardcaml
open! Hardcaml_wr
open! Signal

module I = struct
  type 'a t =
    { rx : 'a Stream.Byte.t
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

type 'a t = { tx : 'a Stream.Byte.t } [@@deriving hardcaml]

module Ethernet_parser = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Stream.Byte.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { dst_mac : 'a [@bits 6 * 8]
      ; src_mac : 'a [@bits 6 * 8]
      ; ethertype : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end
end

let ones_complement_add' (type c) (module C : Comb.S with type t = c) (a : c) (b : c) =
  let open C in
  [%test_eq: int] (width a) (width b);
  let carry_sum = (gnd @: a) +: (gnd @: b) in
  let carry = msb carry_sum in
  let sum = lsbs carry_sum in
  mux2 carry (mux2 (all_bits_set sum) (one (width a)) (incr sum)) sum
;;

let ones_complement_add a b = ones_complement_add' (module Signal) a b
