(** Represent precision of a Q-format value.

    The sexp serialization use the ARM Q-format notation: a single atom of the form
    [uQ<i>.<f>] for unsigned precisions and [Q<i>.<f>] for signed precisions. *)

open Core

type t =
  { signed : bool
  ; i_bits : int
  ; f_bits : int
  }
[@@deriving sexp, compare ~localize, equal ~localize, hash]

(** Return the width (in bits) of the value with given precision. *)
val width : t -> int

(** Return a string representation of the precision (used for printing).

    When [space_padding_for_signed] is true, if [t.signed] is set, it will print " Q1.10"
    instead of "Q1.10" *)
val to_string : ?space_padding_for_signed:bool (** Defaults to [true] *) -> t -> string

(** The precision of a boolean. Commonly used. *)
val bool : t

(** The precision of an unsigned int. Commonly used. *)
val unsigned_int : int -> t

(** The precision of a signed int. Commonly used. *)
val signed_int : int -> t

(** Check whether the precision represents an int. Commonly used. *)
val is_int : t -> bool

(** Check whether the precision represents an unsigned int. Commonly used. *)
val is_unsigned_int : t -> bool

(** Check whether the precision represents a signed int. Commonly used. *)
val is_signed_int : t -> bool

(** Converts an unsigned precision to the minimum-width compatible signed precision.
    Raises if the input is signed already. *)
val to_signed_exn : t -> t

val min_representable_value_as_float : t -> float
val max_representable_value_as_float : t -> float

(** {2 Quickcheck Generators} *)

val gen_random_value_in_precision_as_float
  :  ?must_be_positive:bool (** Defaults to false *)
  -> t
  -> float Quickcheck.Generator.t
