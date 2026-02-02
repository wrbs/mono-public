open! Core
open! Hardcaml

type t =
  { write_ready : Signal.t
  ; read_ready : Signal.t
  ; read_data : Signal.t
  ; length : Signal.t
  ; is_full : Signal.t (* alias for not write_ready *)
  ; is_empty : Signal.t (* alias for not read_ready *)
  }

val create
  :  Scope.t
  -> Signal.Reg_spec.t
  -> capacity:int
  -> write_data:Signal.t
  -> write_valid:Signal.t
  -> reading:Signal.t
  -> t
