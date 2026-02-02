open! Core
open! Hardcaml

module I : sig
  type 'a t =
    { enable : 'a
    ; irq : 'a
    ; nmi : 'a
    ; data : 'a
    ; clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml]
end

module Mem_port : sig
  type 'a t =
    { addr : 'a
    ; data : 'a
    ; write : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { mem : 'a Mem_port.t
    ; state : 'a
    ; fetching : 'a
    ; pc : 'a
    ; a : 'a
    ; s : 'a
    ; x : 'a
    ; y : 'a
    ; p : 'a
    ; illegal : 'a
    }
  [@@deriving hardcaml]
end

module State : sig
  type t [@@deriving sexp_of, enumerate]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
