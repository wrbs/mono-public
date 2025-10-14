@@ portable

(** References to unique values, like [std::Cell] in Rust.

    Allows creating multiple references to a single [unique] value. Accessing the stored
    value requires either [unique] access to the reference, or inserting another value
    into the reference to replace it. *)

type (!'a : value_or_null) t : mutable_data with 'a

(** [make a] creates a new reference containing the given value [a]. *)
external make
  : ('a : value_or_null).
  'a @ once unique -> ('a t[@local_opt]) @ unique
  = "%makemutable"

(** [get t] destroys [t] to extract the value inside. *)
external get
  : ('a : value_or_null).
  ('a t[@local_opt]) @ unique -> 'a @ once unique
  = "%field0"

(** [set t a] overrides the stored value inside [t] with [a]. *)
external set
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a @ once unique -> unit
  = "%setfield0"

(** [exchange t a] extracts the value inside [t], replacing it with [a]. *)
val exchange : ('a : value_or_null). 'a t @ local -> 'a @ once unique -> 'a @ once unique
