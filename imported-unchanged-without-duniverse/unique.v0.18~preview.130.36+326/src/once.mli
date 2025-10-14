@@ portable

(** Dynamically checked references that can only be accessed once. *)
type 'a t

(** [make a] creates a reference to a [once unique] value [a]. *)
val make : 'a @ once unique -> 'a t

(** [get_or_null t] returns the value inside [t], or [Null] if it was already accessed. *)
val get_or_null : 'a t @ local -> 'a or_null @ once unique

(** [get_exn t] returns the value inside [t], or raises [Failure] if it was already
    accessed. *)
val get_exn : 'a t @ local -> 'a @ once unique

module Atomic : sig
  type (!'a : value) t : immutable_data with 'a @@ contended portable

  (** [make a] creates a reference to a [once unique] value [a]. *)
  val make : 'a @ contended once portable unique -> 'a t

  (** [get_opt t] returns the value inside [t], or [None] if it was already accessed. *)
  val get_opt : ('a : value). 'a t @ local -> 'a option @ contended once portable unique

  (** [get_exn t] returns the value inside [t], or raises [Failure] if it was already
      accessed. *)
  val get_exn : ('a : value). 'a t @ local -> 'a @ contended once portable unique
end
