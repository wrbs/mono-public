open! Core
open! Hardcaml

module type S = sig
  type base
  type validity

  type t = private
    { value : base
    ; validity : validity
    }
  [@@deriving sexp_of, equal ~localize]

  (** Creators *)

  val create : base -> valid:base -> t
  val create_valid : base -> t
  val create_with_mask : base -> valid_mask:base -> t
  val of_with_valid : base With_valid.t -> t

  (* Getters *)

  val value : t -> base
  val is_valid : t -> base
  val to_with_valid : t -> base With_valid.t
  val valid_mask : t -> base
  val to_static_known : t -> bool option
  val is_known_valid : t -> bool

  (** Custom functions *)

  val lift : (base -> base) -> t -> t
  val lift2 : (base -> base -> base) -> t -> t -> t
  val and_valid : t -> valid:base -> t
  val or_valid : t -> valid:base -> t

  include Comb.S with type t := t

  val __ppx_auto_name : loc:[%call_pos] -> t -> string -> t
end

module type Validated = sig
  module type S = S

  module Make (C : Comb.S) : S with type base := C.t
  module Bits : S with type base := Bits.t
  include S with type base := Signal.t
end
