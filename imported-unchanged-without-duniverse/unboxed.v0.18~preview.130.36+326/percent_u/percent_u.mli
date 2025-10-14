open! Core
open Import

type t : float64 mod everything [@@deriving quickcheck]

val globalize : local_ t -> t

(** Serializers *)

val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
val to_string : t -> string
val of_string : string -> t

include%template Bin_prot.Binable.S_any [@mode local] with type t := t

include Ppx_hash_lib.Hashable.S_any with type t := t

val typerep_of_t : t Typerep.t
val box : t -> Percent.t
val unbox : local_ Percent.t -> t [@@zero_alloc]

module O : sig
  (** Infix *)

  val ( * ) : t -> t -> t [@@zero_alloc]
  val ( + ) : t -> t -> t [@@zero_alloc]
  val ( - ) : t -> t -> t [@@zero_alloc]
  val ( / ) : t -> t -> t [@@zero_alloc]
  val ( // ) : t -> t -> Float_u.t [@@zero_alloc]

  (** Comparison Infix *)

  val ( >= ) : t -> t -> bool [@@zero_alloc]
  val ( <= ) : t -> t -> bool [@@zero_alloc]
  val ( = ) : t -> t -> bool [@@zero_alloc]
  val ( > ) : t -> t -> bool [@@zero_alloc]
  val ( < ) : t -> t -> bool [@@zero_alloc]
  val ( <> ) : t -> t -> bool [@@zero_alloc]
end

include module type of O

(** Comparison *)

val equal : t -> t -> bool [@@zero_alloc]
val compare : t -> t -> int [@@zero_alloc]
val min : t -> t -> t [@@zero_alloc]
val max : t -> t -> t [@@zero_alloc]
val ascending : t -> t -> int
val descending : t -> t -> int
val between : t -> low:t -> high:t -> bool
val clamp_exn : t -> min:t -> max:t -> t
val is_nan : t -> bool [@@zero_alloc]

(** Conversion

    The [_approx] functions below avoid float division. *)

val of_mult : Float_u.t -> t [@@zero_alloc]
val to_mult : t -> Float_u.t [@@zero_alloc]
val of_percentage : Float_u.t -> t [@@zero_alloc]
val of_percentage_approx : t -> Float_u.t [@@zero_alloc]
val to_percentage : t -> Float_u.t [@@zero_alloc]
val of_bp : Float_u.t -> t [@@zero_alloc]
val of_bp_approx : Float_u.t -> t [@@zero_alloc]
val to_bp : t -> Float_u.t [@@zero_alloc]
val of_bp_int : int -> t [@@zero_alloc]
val of_bp_int_approx : int -> t [@@zero_alloc]
val to_bp_int : t -> int [@@zero_alloc]

(** Misc *)

val zero : unit -> t [@@zero_alloc]
val one_hundred_percent : unit -> t [@@zero_alloc]
val apply : t -> Float_u.t -> Float_u.t [@@zero_alloc]
val scale : t -> Float_u.t -> t [@@zero_alloc]

module Option : sig
  type value := t
  type t : float64 mod everything

  val globalize : local_ t -> t
  val box : t -> Percent.Option.t
  val unbox : local_ Percent.Option.t -> t [@@zero_alloc]
  val to_string : t -> string
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t

  include%template Bin_prot.Binable.S_any [@mode local] with type t := t

  include Ppx_hash_lib.Hashable.S_any with type t := t

  val typerep_of_t : t Typerep.t
  val none : unit -> t [@@zero_alloc]
  val is_none : t -> bool [@@zero_alloc]
  val is_some : t -> bool [@@zero_alloc]
  val some : value -> t [@@zero_alloc]

  (* Conversion *)
  val of_mult : UFO.t -> t [@@zero_alloc]
  val to_mult : t -> UFO.t [@@zero_alloc]
  val of_percentage : UFO.t -> t [@@zero_alloc]
  val to_percentage : t -> UFO.t [@@zero_alloc]
  val of_percentage_approx : UFO.t -> t [@@zero_alloc]
  val of_bp : UFO.t -> t [@@zero_alloc]
  val to_bp : t -> UFO.t [@@zero_alloc]
  val of_bp_approx : UFO.t -> t [@@zero_alloc]
  val apply : t -> UFO.t -> UFO.t [@@zero_alloc]
  val scale : t -> UFO.t -> t [@@zero_alloc]
  val equal : t -> t -> bool [@@zero_alloc]
  val equal__local : local_ t -> local_ t -> bool [@@zero_alloc]
  val compare : t -> t -> int [@@zero_alloc]

  (** [unchecked_some] will not validate that [value] is a valid [Some] so the [t]
      returned may be [none]. *)
  val unchecked_some : value -> t
  [@@zero_alloc]

  module Optional_syntax : sig
    module Optional_syntax : sig
      val is_none : t -> bool [@@zero_alloc]
      val unsafe_value : t -> value [@@zero_alloc]
    end
  end

  module O : sig
    (** Infix *)

    val ( * ) : t -> t -> t [@@zero_alloc]
    val ( + ) : t -> t -> t [@@zero_alloc]
    val ( - ) : t -> t -> t [@@zero_alloc]
    val ( / ) : t -> t -> t [@@zero_alloc]

    (** Comparison Infix *)

    val ( >= ) : t -> t -> bool [@@zero_alloc]
    val ( <= ) : t -> t -> bool [@@zero_alloc]
    val ( = ) : t -> t -> bool [@@zero_alloc]
    val ( > ) : t -> t -> bool [@@zero_alloc]
    val ( < ) : t -> t -> bool [@@zero_alloc]
    val ( <> ) : t -> t -> bool [@@zero_alloc]
  end

  include module type of O
end

module Stable : sig
  module V1 : sig
    type nonrec t = t

    val sexp_of_t : t -> Sexp.t
    val t_of_sexp : Sexp.t -> t

    include%template Bin_prot.Binable.S_any [@mode local] with type t := t
  end

  module Option : sig
    module V1 : sig
      type nonrec t = Option.t

      val sexp_of_t : t -> Sexp.t
      val t_of_sexp : Sexp.t -> t

      include%template Bin_prot.Binable.S_any [@mode local] with type t := t
    end
  end
end
