@@ portable

open! Core

module Kind : sig
  type t =
    | Note_on
    | Note_off
    | Aftertouch
    | Controller
    | Program_change
    | Pressure
    | Pitch_wheel
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]

  module Single : sig
    type t =
      | Program_change
      | Pressure
    [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]
  end

  module Double : sig
    type t =
      | Note_on
      | Note_off
      | Aftertouch
      | Controller
      | Pitch_wheel
    [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]
  end

  module Typed : sig
    type _ t =
      | Single : Single.t -> Value.t t
      | Double : Double.t -> (Value.t * Value.t) t

    type packed = T : _ t -> packed
  end

  val to_typed : t -> Typed.packed
end

type t =
  | Note_on of
      { note : Value.t
      ; velocity : Value.t
      }
  | Note_off of
      { note : Value.t
      ; velocity : Value.t
      }
  | Aftertouch of
      { note : Value.t
      ; pressure : Value.t
      }
  | Controller of
      { controller : Value.t
      ; value : Value.t
      }
  | Program_change of Value.t
  | Pressure of Value.t
  | Pitch_wheel of Value.Double.t
[@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

val kind : t -> Kind.t
val payload_length : t -> int
val payload : t -> Value.t * Value.t option
val decode_payload : 'a Kind.Typed.t -> 'a -> t

module Status : sig
  type t = Channel.t * Kind.t
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]

  val to_byte : t -> Byte.t
  val of_byte : Byte.t -> t option
end

module With_channel : sig
  type nonrec t = Channel.t * t
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

  val status : t -> Status.t
  val length : ?running_status:Status.t -> t -> int
  val encode : ?running_status:Status.t -> t -> f:(Byte.t -> unit) -> unit
  val to_string : ?running_status:Status.t -> t -> Byte.String.t
  val parse_string : ?running_status:Status.t -> Byte.String.t -> t option
end
