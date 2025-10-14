(** [Packed_float_option.t] is the moral equivalent of [float option] represented under
    the hood as a regular float where [Packed_float_option.is_none = Float.is_nan].

    This type is often used whenever we have code dealing with floating point numbers in
    code and we want to avoid allocating on the OCaml heap. *)

open! Core

type t = private float
[@@deriving bin_io ~localize, globalize, sexp, sexp_grammar, typerep]

(** This interface includes [hash]. However, the evaluation of [hash t] requires a
    comparison, and also it allocates, so this should not be used in highly
    performance-sensitive applications. *)
include%template
  Immediate_option.S_without_immediate_zero_alloc
  [@mode local]
  with type t := t
   and type value := float

include Comparable_binable with type t := t

module Array : sig
  type elt := t
  type t [@@deriving bin_io ~localize, globalize, sexp]

  include Float_array.S with type t := t and type float_elt := elt

  module Permissioned :
    Float_array.Permissioned with type permissionless := t and type float_elt := elt

  (** Like [of_float_nan_as_none], except it reinterprets the array. Note that this is a
      view into the array, and no copy is created. **)
  val view_of_float_array_nan_as_none : Float_array.t -> t

  (** Like [to_float_none_as_nan], except it reinterprets the array. Note that this is a
      view into the array, and no copy is created. *)
  val view_to_float_array_none_as_nan : t -> Float_array.t
end

val zero : t

(** Unlike polymorphic compare, [none=none]. *)
val equal : t -> t -> bool

val value_map : t -> f:(float -> 'a) -> default:'a -> 'a

(** In some applications, float arthimetic is done where [nan] is used to denote an
    invalid value. This function can be used to interpret such results as a [t] with no
    overhead (internally, both this and [unchecked_value] are just the identity). *)
val of_float_nan_as_none : float -> t
[@@zero_alloc strict]

val to_float_none_as_nan : t -> float

(** The result of arithmetic operations will be [none] if the operand is. In addition, if
    the float operation would give [nan], the result is [none]. *)
val abs : t -> t

val neg : t -> t
val log : t -> t
val log10 : t -> t
val log1p : t -> t
val sqrt : t -> t
val square : t -> t
val exp : t -> t

(** The result of [is_inf] will be: [true]: if operand is [some inf] or [some -inf];
    [false]: otherwise, including when operand is [none] *)
val is_inf : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_positive val]
   [false]:  otherwise, including when operand is [none]
*)
val is_positive : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_non_positive val]
   [false]: otherwise, including when operand is [none]
*)
val is_non_positive : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_negative val]
   [false]:  otherwise, including when operand is [none]
*)
val is_negative : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_non_negative val]
   [false]: otherwise, including when operand is [none]
*)
val is_non_negative : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_integer val]
   [false]:  otherwise, including when operand is [none]
*)
val is_integer : t -> bool

(* Returns:
   [true]: if operand is [some val], such that [Float.is_finite val]
   [false]: otherwise, including when operand is [none]
*)
val is_finite : t -> bool

(** The result of [min] and [max] will be [none] if either operand is. *)
val min : t -> t -> t

val max : t -> t -> t
val inv : t -> t
val scale : t -> float -> t

(** [first_some x y] returns x if x is not none, else returns y *)
val first_some : t -> t -> t

(** [merge x y ~f] merges together the values from [x] and [y] using [f]. If both [x] and
    [y] are [None], returns [none]. If only one is [Some], returns that one, and if both
    are [Some], returns the result of applying [f] to the contents of [x] and [y]. *)
val merge : t -> t -> f:(float -> float -> float) -> t

val validate : none:unit Validate.check -> some:float Validate.check -> t Validate.check

val validate_option_bound
  :  may_be_none:bool
  -> ?min:float Maybe_bound.t
  -> ?max:float Maybe_bound.t
  -> t Validate.check

val to_string : t -> string

(** Computes [numerator / denominator] when [denominator] is nonzero, otherwise returns
    [else_]. This is done without allocating or branching by unboxing the arguments and
    then using [Unboxed.select]. *)
val divide_if_denominator_nonzero_else : numerator:t -> denominator:t -> else_:t -> t

module Infix : sig
  (** The result of arithmetic operations will be [none] if either operand is. In
      addition, if the float operation would give [nan], the result is [none]. *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ** ) : t -> t -> t

  (** Returns [false] if either operand is [none]. *)

  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool

  (** Returns true if both operands are [none] or (not [none] and equal). *)

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module Local : sig
  val globalize : local_ t -> t
  val of_float_nan_as_none : local_ float -> local_ t
  val to_float_none_as_nan : local_ t -> local_ float
  val is_none : local_ t -> bool
  val equal : local_ t -> local_ t -> bool

  module Infix : sig
    (** Exposes local_ versions of the infix operators. Comments of [Infix] apply here as
        well. *)

    val ( + ) : local_ t -> local_ t -> local_ t
    val ( - ) : local_ t -> local_ t -> local_ t
    val ( * ) : local_ t -> local_ t -> local_ t
    val ( / ) : local_ t -> local_ t -> local_ t
    val ( < ) : local_ t -> local_ t -> bool
    val ( <= ) : local_ t -> local_ t -> bool
    val ( > ) : local_ t -> local_ t -> bool
    val ( >= ) : local_ t -> local_ t -> bool
    val ( = ) : local_ t -> local_ t -> bool
    val ( <> ) : local_ t -> local_ t -> bool
  end

  include module type of Infix

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : local_ t -> bool [@@zero_alloc]
      val unsafe_value : local_ t -> local_ float [@@zero_alloc]
    end
  end
end

module Stable : sig
  module V1 : sig
    (** [none] compares equal to itself, and less than any [some] value. Comparison is not
        robust. *)
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , stable_witness]
  end
end

(** A variant of the above type using unboxed floats. The operations are largely the same
    (but we can't currently share the module type across layouts.)

    (This interface is somewhat sparser than what you see above; this is mostly not a
    choice and more we haven't implemented everything. Feel free to add!) *)
module Unboxed : sig
  type boxed := t
  type t : float64 mod everything = private float# [@@deriving quickcheck]

  val globalize : local_ t -> t
  val to_string : t -> string
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t

  include%template Bin_prot.Binable.S_any [@mode local] with type t := t

  include Ppx_hash_lib.Hashable.S_any with type t := t

  val typerep_of_t : t Typerep.t
  val none : unit -> t [@@zero_alloc]
  val is_none : t -> bool [@@zero_alloc]
  val is_some : t -> bool [@@zero_alloc]
  val some : float# -> t [@@zero_alloc]
  val unchecked_some : float# -> t [@@zero_alloc]
  val of_float_nan_as_none : float# -> t [@@zero_alloc strict]
  val to_float_none_as_nan : t -> float# [@@zero_alloc]
  val neg : t -> t [@@zero_alloc]
  val zero : unit -> t [@@zero_alloc]
  val one : unit -> t [@@zero_alloc]
  val scale : t -> float# -> t [@@zero_alloc]
  val equal : t -> t -> bool [@@zero_alloc]

  (* Using double underscore as expected by [@deriving equal ~local] *)
  val equal__local : local_ t -> local_ t -> bool [@@zero_alloc]
  val div : t -> float# -> t [@@zero_alloc]
  val abs : t -> t [@@zero_alloc]
  val of_option : float option -> t
  val of_option_local : local_ float option -> t
  val to_option : t -> float option
  val to_option_local : t -> local_ float option

  (** The result of [min] and [max] will be [none] if either operand is. *)
  val min : t -> t -> t
  [@@zero_alloc]

  val max : t -> t -> t [@@zero_alloc]
  val select : bool -> t -> t -> t [@@zero_alloc]
  val some_if : bool -> float# -> t [@@zero_alloc]

  (** A workaround for the lack of unboxed float literals; strictly speaking one can use
      it for non constants but the name is a helpful reminder. *)
  val const : float -> t
  [@@zero_alloc]

  (** Converters for the normal PFO. *)
  val box : t -> boxed

  val box_local : local_ t -> local_ boxed
  val unbox : local_ boxed -> t [@@zero_alloc]
  val unbox_local : local_ boxed -> t [@@zero_alloc]

  (** [first_some x y] returns x if x is not none, else returns y *)
  val first_some : t -> t -> t
  [@@zero_alloc]

  (** [some_or t ~default = first_some t (unchecked_some t)] *)
  val some_or : t -> default:float# -> t
  [@@zero_alloc]

  (** Computes [numerator / denominator] when [denominator] is nonzero, otherwise returns
      [else_]. This is done without allocating or branching by using [Unboxed.select]. *)
  val divide_if_denominator_nonzero_else : numerator:t -> denominator:t -> else_:t -> t
  [@@zero_alloc]

  val value : t -> default:Float_u.t -> Float_u.t [@@zero_alloc]
  val value_exn : t -> Float_u.t [@@zero_alloc]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool [@@zero_alloc]
      val unsafe_value : t -> Float_u.t [@@zero_alloc]
    end
  end

  val compare : t -> t -> int [@@zero_alloc]

  module Infix : sig
    val ( + ) : t -> t -> t [@@zero_alloc]
    val ( - ) : t -> t -> t [@@zero_alloc]
    val ( * ) : t -> t -> t [@@zero_alloc]
    val ( / ) : t -> t -> t [@@zero_alloc]
    val ( ** ) : t -> t -> t [@@zero_alloc]
    val ( < ) : t -> t -> bool [@@zero_alloc]
    val ( <= ) : t -> t -> bool [@@zero_alloc]
    val ( > ) : t -> t -> bool [@@zero_alloc]
    val ( >= ) : t -> t -> bool [@@zero_alloc]
    val ( = ) : t -> t -> bool [@@zero_alloc]
    val ( <> ) : t -> t -> bool [@@zero_alloc]
  end

  module O = Infix

  module Array : sig
    include Float_u.Array with type elt := t

    val sexp_of_t : t -> Sexp.t
  end

  val merge : t -> t -> f:(float# -> float# -> float#) -> t

  module Ref : sig
    type elt := t
    type t

    val create : elt -> t
    val create_local : elt -> local_ t [@@zero_alloc]
    val create_none : unit -> t
    val create_zero : unit -> t
    val get : local_ t -> elt [@@zero_alloc]
    val set : local_ t -> elt -> unit [@@zero_alloc]
    val set_none : local_ t -> unit [@@zero_alloc]
    val add : local_ t -> elt -> unit

    module O : sig
      val ref : elt -> local_ t
      val ( ! ) : local_ t -> elt [@@zero_alloc]
      val ( := ) : local_ t -> elt -> unit [@@zero_alloc]
      val ( += ) : local_ t -> elt -> unit [@@zero_alloc]
    end
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving globalize, stable_witness]

      (** We derive [sexp], [bin_io], [hash], [typerep], [equal], and [compare]. *)

      val sexp_of_t : t -> Sexp.t
      val t_of_sexp : Sexp.t -> t

      include%template Bin_prot.Binable.S_any [@mode local] with type t := t

      include Ppx_hash_lib.Hashable.S_any with type t := t

      val typerep_of_t : t Typerep.t
      val equal : t -> t -> bool
      val compare : t -> t -> int
    end
  end
end
