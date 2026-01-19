open! Core
open! Hardcaml

val create
  :  ?memory_size:int
  -> Signal.t
  -> width:int
  -> clock:Hardcaml.Signal.t
  -> enable:Hardcaml.Signal.t
  -> reset:Hardcaml.Signal.t
  -> init_file:string
  -> Hardcaml.Signal.t
