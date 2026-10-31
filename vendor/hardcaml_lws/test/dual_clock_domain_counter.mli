open Hardcaml

module I : sig
  type 'a t =
    { clock_a : 'a
    ; clock_b : 'a
    ; enable : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { count_a : 'a
    ; count_b : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
