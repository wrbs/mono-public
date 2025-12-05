@@ portable

open! Core

module Fps : sig
  type t =
    | Fps24
    | Fps25
    | Fps29
    | Fps30
  [@@deriving sexp_of, compare ~localize, equal ~localize, enumerate, quickcheck]

  val to_int : t -> int
  val of_int : int -> t option
end

module Time : sig
  type t = private
    { hour : int
    ; minute : int
    ; second : int
    ; frame : int
    ; subframe : int
    ; fps : Fps.t
    }
  [@@deriving sexp_of, compare ~localize, equal ~localize, quickcheck]

  val create
    :  ?hour:int
    -> ?minute:int
    -> ?second:int
    -> ?frame:int
    -> ?subframe:int
    -> Fps.t
    -> t option

  val of_bytes : hr:Byte.t -> mn:Byte.t -> se:Byte.t -> fr:Byte.t -> ff:Byte.t -> t option
end
