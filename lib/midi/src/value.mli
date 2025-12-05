@@ portable

open! Core

type t = private int [@@deriving compare, quickcheck, enumerate]

include Identifiable.S [@mode local] with type t := t

val to_byte : t -> Byte.t
val of_byte : Byte.t -> t option
val of_byte_exn : Byte.t -> t
val to_int : t -> int
val of_int : int -> t option
val of_int_exn : int -> t
val min_value : t
val max_value : t
val min_int : int
val max_int : int

module Encoding : sig
  type _ t =
    | Int_clamp : int t
    | Float : float t
    | Float_pm : float t
    | Float_range : (float * float) -> float t
    | Percent : Percent.t t
    | Bool : bool t
end

val encode : 'a Encoding.t -> 'a -> t
val decode : 'a Encoding.t -> t -> 'a

module Double : sig
  type value := t
  type t = private int [@@deriving quickcheck]

  include Identifiable.S [@mode local] with type t := t

  val of_values : hi:value -> lo:value -> t
  val to_values : t -> hi:value * lo:value
  val to_int : t -> int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val min_value : t
  val max_value : t
  val min_int : int
  val max_int : int
  val encode : 'a Encoding.t -> 'a -> t
  val decode : 'a Encoding.t -> t -> 'a
end
