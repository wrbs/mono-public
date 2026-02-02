open! Core
open! Hardcaml

module Nes_master_clock : sig
  module I : sig
    type 'a t =
      { clock_100 : 'a
      ; reset : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { clock : 'a
      ; locked : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end
