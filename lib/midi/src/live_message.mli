@@ portable

open! Core

type t =
  | MIDI of Message.With_channel.t
  | Sysex of Value.t iarray
  | Mtc_quarter_frame of Value.t
  | Song_position of Value.Double.t
  | Song_select of Value.t
  | Tune_request
  | Realtime of Status.Realtime.t
[@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

val payload_length : t -> int
val length : t -> int
val start_status : t -> Status.t
val encode' : t -> f:(Parsed_byte.t -> unit) -> unit
val encode : t -> f:(Byte.t -> unit) -> unit
val to_iarray : t -> Parsed_byte.t iarray
val to_string : t -> string
val to_string_many : t iarray -> running_status:bool -> string
val parse_string : string -> t iarray

(* Creators *)

val note_on : Value.t -> velocity:Value.t -> channel:Channel.t -> t
val note_off : ?velocity:Value.t -> Value.t -> channel:Channel.t -> t
val aftertouch : Value.t -> pressure:Value.t -> channel:Channel.t -> t
val cc : Value.t -> controller:Value.t -> channel:Channel.t -> t
val program_change : Value.t -> channel:Channel.t -> t
val pressure : Value.t -> channel:Channel.t -> t
val pitch_wheel : Value.Double.t -> channel:Channel.t -> t
val sysex : Value.t iarray -> t
val mtc_quarter_frame : Value.t -> t
val song_position : Value.Double.t -> t
val song_select : Value.t -> t
val tune_request : t
val clock : t
val tick : t
val start : t
val continue : t
val stop : t
val active_sense : t
val reset : t
val panic : unit -> t Collection.t

type message := t

module Running_status : sig
  type t = Message.Status.t option [@@deriving equal ~localize, sexp_of]

  val initial : t
  val length : t -> message -> int
  val encode' : t -> message -> f:(Parsed_byte.t -> unit) -> t
  val encode : t -> message -> f:(Byte.t -> unit) -> t
  val to_iarray : t -> message -> Parsed_byte.t iarray * t
  val to_string : t -> message -> string * t
  val to_string_many : t -> message iarray -> string * t
end

module Parser : sig
  module Immutable : sig
    type t : immutable_data [@@deriving sexp_of]

    val initial : t
    val add' : t -> Parsed_byte.t -> message option * t
    val add : t -> Byte.t -> message option * t
  end

  type t : mutable_data [@@deriving sexp_of]

  val create : unit -> t
  val add' : t -> Parsed_byte.t -> message option
  val add : t -> Byte.t -> message option
end
