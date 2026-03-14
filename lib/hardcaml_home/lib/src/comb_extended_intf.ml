open! Core
open! Hardcaml

module type S = sig
  type t

  val shift_into : t -> add:t -> at:[ `msb | `lsb ] -> t
  val split_out : t -> width:int -> from:[ `msb | `lsb ] -> t * t
  val ones_complement_add : t -> t -> t
end

module type Comb_extended = sig
  module Make (C : Comb.S) : S with type t := C.t
  module Bits_extended : S with type t := Bits.t

  module Signal_extended : sig
    include S with type t := Signal.t
    open Signal

    module Always_extended : sig
      val shift_into_var : Always.Variable.t -> add:t -> at:[ `msb | `lsb ] -> Always.t
    end
  end
end
