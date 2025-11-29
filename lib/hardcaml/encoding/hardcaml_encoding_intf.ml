open! Core
open! Hardcaml

module type S_encodable = sig
  type t [@@deriving compare ~localize, enumerate, sexp_of]

  val width : int
  val to_int : t -> int
end

module type S_enc_with_comb = sig
  type case
  type t
  type comb

  val const : case -> t
  val of_raw : comb -> t
  val of_raw_with_valid : comb -> (comb, t) With_valid.t2
  val is : case -> t -> comb
  val is_valid : t -> comb
  val with_valid : t -> (comb, t) With_valid.t2
  val ( ==: ) : t -> t -> comb
  val match_ : t -> (case option -> comb) -> comb

  (** Raises if the encoding isn't infallible *)
  val mux_exn : t -> (case -> comb) -> comb
end

module type S_enc = sig
  type case
  type 'a t = private { raw : 'a } [@@unboxed]

  include Interface.S with type 'a t := 'a t

  val ast : Interface.Ast.t
  val to_raw : 'a t -> 'a

  (** Decoding bits *)

  val decode : Bits.t t -> case option
  val decode_or_error : Bits.t t -> case Or_error.t
  val decode_exn : Bits.t t -> case

  (** Encoding aliases *)

  val of_bits : Bits.t -> Bits.t t
  val of_signal : Signal.t -> Signal.t t
  val bits : case -> Bits.t t
  val signal : case -> Signal.t t

  (** Matching *)

  val match_bits : Bits.t t -> (case option -> Bits.t) -> Bits.t
  val match_signal : Signal.t t -> (case option -> Signal.t) -> Signal.t
  val switch_list : Signal.t t -> (case option -> Always.t list) -> Always.t list
  val switch : Signal.t t -> (case option -> Always.t list) -> Always.t
  val switch' : Signal.t t -> (case -> Always.t list) -> Always.t

  (** Sim *)

  val sim_set : Bits.t ref t -> case -> unit
  val sim_set_raw : Bits.t ref t -> Bits.t -> unit
  val sim_get : Bits.t ref t -> case option
  val sim_get_or_error : Bits.t ref t -> case Or_error.t
  val sim_get_exn : Bits.t ref t -> case
  val sim_get_raw : Bits.t ref t -> Bits.t

  (** Functions for a [Comb.S]: see also [Make_comb] or [Of_bits/[Of_signal] *)

  val const : (module Comb.S with type t = 'a) -> case -> 'a t
  val of_raw : (module Comb.S with type t = 'a) -> 'a -> 'a t

  val of_raw_with_valid
    :  (module Comb.S with type t = 'a)
    -> 'a
    -> ('a, 'a t) With_valid.t2

  val is : (module Comb.S with type t = 'a) -> case -> 'a t -> 'a
  val is_valid : (module Comb.S with type t = 'a) -> 'a t -> 'a
  val with_valid : (module Comb.S with type t = 'a) -> 'a t -> ('a, 'a t) With_valid.t2
  val ( ==: ) : (module Comb.S with type t = 'a) -> 'a t -> 'a t -> 'a
  val match_ : (module Comb.S with type t = 'a) -> 'a t -> (case option -> 'a) -> 'a
  val mux_exn : (module Comb.S with type t = 'a) -> 'a t -> (case -> 'a) -> 'a

  module Make_comb (X : Comb.S) : sig
    include module type of Make_comb (X) (** @inline *)

    (** @inline *)
    include S_enc_with_comb with type case := case and type t := t and type comb := X.t
  end

  module Of_bits : sig
    include module type of Of_bits (** @inline *)

    (** @inline *)
    include S_enc_with_comb with type case := case and type t := t and type comb := Bits.t
  end

  module Of_signal : sig
    include module type of Of_signal (** @inline *)

    (** @inline *)
    include
      S_enc_with_comb with type case := case and type t := t and type comb := Signal.t

    val reg_initial : Signal.Reg_spec.t -> initial:case -> t
  end

  module Of_always : sig
    include module type of Of_always (** @inline *)

    val set_to : Always.Variable.t t -> case -> Always.t
    val reg_initial : Signal.Reg_spec.t -> initial:case -> Always.Variable.t t
    val wire' : Signal.t t -> Always.Variable.t t
    val switch_list : Signal.t t -> (case option -> Always.t list) -> Always.t list
    val switch : Signal.t t -> (case option -> Always.t list) -> Always.t
    val switch' : Signal.t t -> (case -> Always.t list) -> Always.t
  end
end

module type S = sig
  type t

  include S_encodable with type t := t

  val of_int : int -> t option

  module Enc : S_enc with type case := t

  (** Matching raw values, avoid round-tripping through an Enc *)

  val to_bits : t -> Bits.t
  val to_signal : t -> Signal.t
  val of_bits : Bits.t -> t option
  val of_bits_or_error : Bits.t -> t Or_error.t
  val of_bits_exn : Bits.t -> t
  val match_bits : Bits.t -> (t option -> Bits.t) -> Bits.t
  val match_signal : Signal.t -> (t option -> Signal.t) -> Signal.t
  val switch : Signal.t -> (t option -> Always.t list) -> Always.t
  val switch' : Signal.t -> (t -> Always.t list) -> Always.t
  val switch_list : Signal.t -> (t option -> Always.t list) -> Always.t list
end

module type S_enums = sig
  type t

  module Binary : S with type t = t
  module One_hot : S with type t = t
end

module type Hardcaml_encoding = sig
  module type S_encodable = S_encodable
  module type S = S
  module type S_enums = S_enums

  module Make (M : S_encodable) : S with type t := M.t
  module Make_binary (M : Enum.Cases) : S with type t := M.t
  module Make_one_hot (M : Enum.Cases) : S with type t := M.t
  module Make_enum (M : Enum.Cases) : S_enums with type t := M.t
end
