open! Core
open! Hardcaml

val update' : (module Comb.S with type t = 'c) -> 'c -> polynomial:Bits.t -> crc:'c -> 'c
val update : Signal.t -> polynomial:Bits.t -> crc:Signal.t -> Signal.t
val update_bits : Bits.t -> polynomial:Bits.t -> crc:Bits.t -> Bits.t

(* Polynomials *)

val crc32 : Bits.t

(* For checking *)

val expected_residual : polynomial:Bits.t -> Bits.t
val add_crc32_le : Bits.t -> Bits.t
