open! Core
open! Hardcaml
open! Signal

let ones_complement_add' (type c) (module C : Comb.S with type t = c) (a : c) (b : c) =
  let open C in
  [%test_eq: int] (width a) (width b);
  let carry_sum = (gnd @: a) +: (gnd @: b) in
  let carry = msb carry_sum in
  let sum = lsbs carry_sum in
  mux2 carry (mux2 (all_bits_set sum) (one (width a)) (incr sum)) sum
;;

let ones_complement_add a b = ones_complement_add' (module Signal) a b
