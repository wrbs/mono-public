open! Core
module Bonsai := Bonsai.Cont
module Effect := Bonsai.Effect

(** Time/samples since startup. *)

type t [@@deriving sexp_of]

val since_start : t -> Time_ns.Span.t
val of_span_since_start : Time_ns.Span.t -> t
val secs_since_start : t -> float
val sub : t -> t -> Time_ns.Span.t
val add : t -> Time_ns.Span.t -> t

(** Gets the current time, updated every main loop (i.e. every block_size
    samples) *)
val current : Bonsai.graph -> t Bonsai.t

(** Gets the current time, updated approximately every [tick_every] *)
val approximate : tick_every:Time_ns.Span.t -> Bonsai.graph -> t Bonsai.t

(** Wrappers of bonsai clock operations in terms of [t] *)

val at : t Bonsai.t -> Bonsai.graph -> Bonsai.Clock.Before_or_after.t Bonsai.t
val get_current : Bonsai.graph -> t Effect.t Bonsai.t
val sleep : Bonsai.graph -> (Time_ns.Span.t -> unit Effect.t) Bonsai.t
val until : Bonsai.graph -> (t -> unit Effect.t) Bonsai.t

module Expert : sig
  (** Internally represented as ns since epoch. These functions convert *)

  val to_time_ns : t -> Time_ns.t
  val of_time_ns : Time_ns.t -> t
end
