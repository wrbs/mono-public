open! Core
open! Hardcaml

module Make (C : Comb.S) : Comb_extended_intf.S with type t := C.t = struct
  open C

  type side =
    [ `msb
    | `lsb
    ]

  let split_out t ~width ~(from : side) =
    match from with
    | `msb -> sel_top t ~width, drop_top t ~width
    | `lsb -> drop_bottom t ~width, sel_bottom t ~width
  ;;

  let shift_into t ~add ~at =
    match at with
    | `msb -> add @: drop_bottom t ~width:(width add)
    | `lsb -> drop_top t ~width:(width add) @: add
  ;;

  let ones_complement_add a b =
    [%test_eq: int] (width a) (width b);
    let carry_sum = (gnd @: a) +: (gnd @: b) in
    let carry = msb carry_sum in
    let sum = lsbs carry_sum in
    mux2 carry (mux2 (all_bits_set sum) (one (width a)) (incr sum)) sum
  ;;
end

module Bits_extended = Make (Bits)

module Signal_extended = struct
  open! Signal
  include Make (Signal)

  module Always_extended = struct
    open Always

    let shift_into_var var ~add ~at = var <-- shift_into var.value ~add ~at
  end
end
