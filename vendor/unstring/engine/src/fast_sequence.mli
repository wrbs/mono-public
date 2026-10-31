open! Core

(** A more defunctionalised, and faster, parsing-oriented alternative to [Sequence.t].

    Operations are represented as data constructors where possible, and zero/one element
    cases are special-cased. Supports memoization via [cache]. *)

type 'a t [@@deriving sexp_of]

val next : 'a t -> ('a * 'a t) option

(** [cache t] returns a sequence that otherwise produces the same stream of values, but
    which caches any values observed, so that they don't need to be recomputed again. *)
val cache : 'a t -> 'a t

(** {1 Constructors} *)

val empty : 'a t
val return : 'a -> 'a t
val cons : 'a -> 'a t Lazy.t -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val bind : 'a t -> f:('a -> 'b t) -> 'b t
val of_list : 'a list -> 'a t
val of_list_map : 'a list -> f:('a -> 'b) -> 'b t
val concat : 'a t list -> 'a t
val recurse : init:'acc -> f:('acc -> [ `Finish of 'a | `Continue of 'acc ] t) -> 'a t
