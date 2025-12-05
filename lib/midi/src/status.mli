@@ portable

open! Core

module Realtime : sig
  type t =
    | Clock
    | Tick
    | Start
    | Continue
    | Stop
    | FD
    | Active_sense
    | Reset
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]
end

type t =
  | MIDI of Message.Status.t
  | Sysex
  | End_sysex
  | Mtc_quarter_frame
  | Song_position
  | Song_select
  | Tune_request
  | Realtime of Realtime.t
  | U_F4
  | U_F5
[@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize, enumerate]

val of_byte : Byte.t -> t option
val of_byte_exn : Byte.t -> t
val to_byte : t -> Byte.t
