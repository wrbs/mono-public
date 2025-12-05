@@ portable

open! Core

module type S = sig @@ portable
  type t = private int [@@deriving quickcheck ~portable]

  include Identifiable.S [@modality portable] [@mode local] with type t := t

  val bits : int
  val to_int : t -> int
  val of_int : int -> t option
  val of_int_exn : int -> t
  val min_value : t
  val max_value : t
  val min_int : int
  val max_int : int
end

module U15 : S
module U28 : S
module U24 : S
