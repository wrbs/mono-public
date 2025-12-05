@@ portable

open! Core

module Format : sig
  type t =
    | Single_track
    | Parallel
    | Sequential
  [@@deriving sexp_of, compare ~localize, equal ~localize, enumerate]
end

module Timing : sig
  type t =
    | Metrical of Num.U15.t
    | Timecode of Smtpe.Fps.t * Byte.t
  [@@deriving sexp_of, compare ~localize, equal ~localize]
end

type t =
  { format : Format.t
  ; timing : Timing.t
  }
[@@deriving sexp_of, compare ~localize, equal ~localize]
