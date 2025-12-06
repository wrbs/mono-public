open! Core
module U15 = Num.U15
module U24 = Num.U24
module U28 = Num.U28
module Event = Event
module Smtpe = Smtpe
module Header = Header
module Io = Io

type t =
  { header : Header.t
  ; tracks : Event.t iarray iarray
  }
[@@deriving sexp_of]

val parse
  :  collectors:
       (Header.t
        -> events:(int -> (Event.t, 'track) Collector.t)
           * tracks:('track, 'result) Collector.t)
  -> 'result Io.Read.t

val read : t Io.Read.t
val of_string : string -> t Or_error.t
