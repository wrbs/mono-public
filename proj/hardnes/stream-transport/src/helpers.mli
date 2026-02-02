open! Core
open! Hardcaml

val ones_complement_add' : (module Comb.S with type t = 'c) -> 'c -> 'c -> 'c
val ones_complement_add : Signal.t -> Signal.t -> Signal.t
