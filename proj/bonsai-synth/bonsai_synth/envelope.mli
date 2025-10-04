open! Core
open! Bonsai_synth_core

module Event : sig
  type t =
    | Start
    | Stop
end

(** An adsr envelope. Returns [value_opt, send_event].

    The reason it returns [None] if the envelope is finished rather than a block of all
    [0.]s is so you can [match%sub] on it to disable computations that will be silenced
    when there isn't a note playing. *)
val adsr
  :  ?attack:Time_ns.Span.t Bonsai.t
  -> ?decay:Time_ns.Span.t Bonsai.t
  -> ?sustain:float Bonsai.t
  -> ?release:Core.Time_ns.Span.t Bonsai.t
  -> local_ Bonsai.graph
  -> Block.t option Bonsai.t * (Event.t -> unit Effect.t) Bonsai.t
