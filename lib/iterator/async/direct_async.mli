open! Core
open! Async

type async
type t = async Handled_effect.Handler.t

val await : t @ local -> 'a Deferred.t -> 'a
val run : (async:t @ local -> 'a) -> 'a Deferred.t
