@@ portable

open! Core

(** A char that string/sexp serializes as a 2-byte hex value *)

type t = char [@@deriving quickcheck, enumerate]

include Identifiable.S [@mode local] with type t := t

external of_char : char -> t = "%identity"
external to_char : t -> char = "%identity"
val min_value : t
val max_value : t
val to_int : t -> int
val of_int : int -> t option
val of_int_exn : int -> t
val of_int_wrap : int -> t

(** [range_incl_* a b] returns all values between [min(a,b)] and [max(a, b)] *)

val range_incl_list : t -> t -> t list
val range_incl_array : t -> t -> t array
val range_incl_iarray : t -> t -> t iarray

module O : sig
  include Comparisons.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( lsl ) : t -> int -> t
  val ( lsr ) : t -> int -> t
  val ( land ) : t -> t -> t
  val ( lor ) : t -> t -> t
  val ( lxor ) : t -> t -> t
  val lnot : t -> t
end

include module type of O

module String : sig
  type t = string [@@deriving quickcheck]

  include Identifiable.S [@mode local] with type t := t
end
