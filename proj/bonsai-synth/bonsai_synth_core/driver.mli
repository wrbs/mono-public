open! Core
open! Async
open! Import

module Session : sig
  type t

  val create : ?on_startup:unit Effect.t Bonsai.t -> Block.t Bonsai.t list -> t
end

val render_to_wav_file
  :  ?sample_rate_hz:int
  -> filename:string
  -> (stop_output:unit Effect.t -> local_ Bonsai.graph -> Session.t)
  -> unit Or_error.t Deferred.t
