open! Core

(** Comparison for type-indexed values that preserves type equality.

    Standard comparison returns [Ordering.t], losing type information. This module
    provides comparison that returns a type equality witness when values are equal,
    enabling type-safe operations on heterogeneous collections. *)
module Ordering : sig
  type ('a, 'b) t =
    | Less
    | Greater
    | Equal of ('a, 'b) Type_equal.t

  val untyped : ('a, 'b) t -> Core.Ordering.t
end

module type S = sig
  type 'a t : value mod non_float [@@deriving sexp_of]

  val compare : 'a t -> 'b t -> ('a, 'b) Ordering.t
end

(** Heterogeneous map with type-indexed keys and values.

    Unlike standard maps where all values have the same type, this map allows the value
    type to depend on the key's type parameter. For example, if [Key.t] is ['a key], then
    the value type can be ['a value]. *)
module Make_map
    (Key : S)
    (Data : sig
       type 'a t [@@deriving sexp_of]
     end) : sig
  type t [@@deriving sexp_of]

  val empty : t
  val find : t -> 'a Key.t -> 'a Data.t option
  val set : t -> key:'a Key.t -> data:'a Data.t -> t
  val keys : t -> ('a. 'a Key.t -> 'b) -> 'b list
  val map : t -> f:('a. 'a Data.t -> 'a Data.t) -> t
end
