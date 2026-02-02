open! Core
open! Hardcaml

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; uart : 'a Byte_stream.Rx.t
    ; init_partial_checksum : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { conf : 'a Network_conf.t
    ; configured : 'a
    ; uart : 'a Byte_stream.Tx.t
    ; partial_checksum : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
