open! Core
module Bonsai := Bonsai.Cont

(** The sample rate in Hz (samples per second).

    Although it's dynamic, you can probably assume the [Bonsai.t]s returned will rarely if
    ever change *)

val float : local_ Bonsai.graph -> float Bonsai.t
val int : local_ Bonsai.graph -> int Bonsai.t
val sample_length : local_ Bonsai.graph -> Time_ns.Span.t Bonsai.t
val sample_length_sec : local_ Bonsai.graph -> float Bonsai.t
val block_length : local_ Bonsai.graph -> Time_ns.Span.t Bonsai.t
val block_length_sec : local_ Bonsai.graph -> float Bonsai.t

module Expert : sig
  val dynamic_scope : int Bonsai.Dynamic_scope.t
end
