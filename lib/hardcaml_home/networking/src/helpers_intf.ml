open! Core
open! Hardcaml

module type S = sig
  type t

  val shift_lsb : t -> add:t -> t
  val shift_msb : t -> add:t -> t
  val ones_complement_add : t -> t -> t
end

module type Helpers = sig
  module Make_helpers (C : Comb.S) : S with type t := C.t
  module Helpers_bits : S with type t := Bits.t
  module Helpers_signal : S with type t := Signal.t
end
