open! Core
open! Hardcaml

module type IO = sig
  module I : Interface.S
  module O : Interface.S
end

module type Basic = sig
  include IO

  val name : string
  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

module type S = sig
  include IO

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val create' : Scope.t -> Signal.t O.t * (Signal.t I.t -> unit)
  val hierarchical : ?instance:string -> Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical' : ?instance:string -> Scope.t -> Signal.t O.t * (Signal.t I.t -> unit)
end

module type Make_S = functor (M : IO) -> S with module I := M.I and module O := M.O

module type Component = sig
  module type IO = IO
  module type Basic = Basic
  module type S = S
  module type Make_S = Make_S

  module Make (M : Basic) : S with module I := M.I and module O := M.O
end
