open! Core

type 'a driver = f:('a -> unit) @ local -> unit
type 'a driven = unit -> 'a option
