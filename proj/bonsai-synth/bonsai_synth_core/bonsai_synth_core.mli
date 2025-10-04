open! Core

(* exports *)
module Block = Block
module Driver = Driver
module Uptime = Uptime
module Sample_rate = Sample_rate

(* re-exports *)
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect

(** [on_tick effect] runs [effect] _after_ every output signal block is computed (64
    samples) i.e. every control-rate tick.

    It's an alias for [Bonsai.Edge.after_display] but that's kind of a confusing name in
    the context of [Bonsai_synth]. *)
val after_tick : unit Effect.t Bonsai.t -> local_ Bonsai.graph -> unit

val after_tick' : unit Effect.t option Bonsai.t -> local_ Bonsai.graph -> unit

(** [stateful f ~init] computes a value with a state retained from tick to tick

    If [f state] returns [(state', value)] then on the next tick (64 samples later) state
    will be [state'].

    Use for oscillators/filters/etc. Internally implemented with [Bonsai.state] and
    [on_tick] *)
val stateful
  :  ('state -> 'state * 'a) Bonsai.t
  -> init:'state
  -> local_ Bonsai.graph
  -> 'a Bonsai.t

(** infix operators for basic signal combination *)

val ( +| ) : here:[%call_pos] -> Block.t Bonsai.t -> Block.t Bonsai.t -> Block.t Bonsai.t
val ( -| ) : here:[%call_pos] -> Block.t Bonsai.t -> Block.t Bonsai.t -> Block.t Bonsai.t
val ( *| ) : here:[%call_pos] -> Block.t Bonsai.t -> Block.t Bonsai.t -> Block.t Bonsai.t
val ( /| ) : here:[%call_pos] -> Block.t Bonsai.t -> Block.t Bonsai.t -> Block.t Bonsai.t
val ( +.| ) : here:[%call_pos] -> Block.t Bonsai.t -> float Bonsai.t -> Block.t Bonsai.t
val ( -.| ) : here:[%call_pos] -> Block.t Bonsai.t -> float Bonsai.t -> Block.t Bonsai.t
val ( *.| ) : here:[%call_pos] -> Block.t Bonsai.t -> float Bonsai.t -> Block.t Bonsai.t
val ( /.| ) : here:[%call_pos] -> Block.t Bonsai.t -> float Bonsai.t -> Block.t Bonsai.t
