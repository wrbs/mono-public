open! Core
module Bonsai := Bonsai.Cont

(** The sample rate in Hz (samples per second). 

    Although it's dynamic, you can probably assume the [Bonsai.t]s returned will
    rarely if ever change *)

val float : Bonsai.graph -> float Bonsai.t
val int : Bonsai.graph -> int Bonsai.t
val sample_length : Bonsai.graph -> Time_ns.Span.t Bonsai.t
val sample_length_sec : Bonsai.graph -> float Bonsai.t
val block_length : Bonsai.graph -> Time_ns.Span.t Bonsai.t
val block_length_sec : Bonsai.graph -> float Bonsai.t

module Expert : sig
  val dynamic_scope : int Bonsai.Dynamic_scope.t
end
