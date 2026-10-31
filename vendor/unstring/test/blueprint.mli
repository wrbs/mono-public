open! Core

(** Randomly-generated parser blueprints for property-based testing.

    A [Blueprint.t] mirrors the public-facing constructors of [Unstring.t] and can be
    converted to either a [Parse.t Unstring.t] (preserving structural parse information)
    or a [unit Unstring.t] (for use with combinators like [star] and [multiple] that
    require unit-typed children). *)

module Char_filter : sig
  type t = Unstring.Char_filter.t [@@deriving quickcheck, sexp_of]

  val char_gen : t -> char Quickcheck.Generator.t option
end

module Bound : sig
  type t = Unstring.Bound.t [@@deriving quickcheck, sexp_of]

  val count_gen : t * t -> int Quickcheck.Generator.t option
end

module Parse : sig
  type t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

type t [@@deriving quickcheck, sexp]

val to_unstring : t -> Parse.t Unstring.t
val to_unit_unstring : t -> unit Unstring.t
val matching_string_gen : t -> string Quickcheck.Generator.t option

(** Generates a string matching [bp] repeated with optional separator and bounds. Useful
    for equivalence tests that need to generate inputs for [star], [multiple], [list], or
    [fold] patterns without duplicating the generation logic. *)
val repeat_matching_string_gen
  :  sep:t option
  -> bounds:(Bound.t * Bound.t) option
  -> t
  -> string Quickcheck.Generator.t option
