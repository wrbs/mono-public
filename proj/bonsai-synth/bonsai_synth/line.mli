open! Core
open! Bonsai_synth_core

type t =
  { value : Block.t Bonsai.t
  ; set : (float -> unit Effect.t) Bonsai.t
  ; queue : ((Time_ns.Span.t * float) list -> unit Effect.t) Bonsai.t
  }

val create : ?init:float -> Bonsai.graph -> t
