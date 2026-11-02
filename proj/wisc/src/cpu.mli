open! Core
open! Hardcaml

module IO : sig
  type 'a t =
    { addr : 'a [@bits 16]
    ; data : 'a [@bits 16]
    ; enable : 'a
    ; write : 'a
    }
  [@@deriving hardcaml]
end

module Registers : sig
  type 'a t =
    { r1 : 'a [@bits 16]
    ; r2 : 'a [@bits 16]
    ; r3 : 'a [@bits 16]
    ; r4 : 'a [@bits 16]
    ; r5 : 'a [@bits 16]
    ; r6 : 'a [@bits 16]
    ; r7 : 'a [@bits 16]
    ; r8 : 'a [@bits 16]
    ; r9 : 'a [@bits 16]
    ; ra : 'a [@bits 16]
    ; rb : 'a [@bits 16]
    ; rc : 'a [@bits 16]
    ; rd : 'a [@bits 16]
    ; re : 'a [@bits 16]
    }
  [@@deriving hardcaml]

  val get : 'a t -> [< Register.real ] -> 'a
end

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; enable : 'a
    ; data_in : 'a [@bits 16]
    ; init_pc : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { io : 'a IO.t
    ; pc : 'a [@bits 16]
    ; registers : 'a Registers.t
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierachichal : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
