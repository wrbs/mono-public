open! Core
open! Async

type async
type t = async Effect.Handler.t

val await : t @ local -> 'a Deferred.t -> 'a
val run : (async:t @ local -> 'a) -> 'a Deferred.t
