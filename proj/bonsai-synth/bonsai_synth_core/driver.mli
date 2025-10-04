open! Core
open! Async
open! Import

module Stop_or_continue : sig
  type t =
    | Stop
    | Continue
end

val render_to_wav_file
  :  ?sample_rate_hz:int
  -> filename:string
  -> (local_ Bonsai.graph -> Block.t Bonsai.t list * Stop_or_continue.t Bonsai.t)
  -> unit Or_error.t Deferred.t
