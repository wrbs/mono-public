open! Core

module type S = sig
  type t = private int [@@deriving compare, equal, string, quickcheck ~generator, sexp]

  module Hex : sig
    type nonrec t = t [@@deriving compare, equal, string, quickcheck ~generator, sexp]
  end

  val to_int : t -> int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val min_int : int
  val max_int : int
end

module type Ints = sig
  module type S = S

  module U4 : S
  module I7 : S
  module I8 : S
  module U16 : S

  module I16 : sig
    include S

    val of_u16 : U16.t -> t
    val to_u16 : t -> U16.t
  end
end
