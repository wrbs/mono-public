open! Core
open Ints

module IO : sig
  type _ t =
    | Read : U16.t -> U16.t t
    | Write : U16.t * U16.t -> unit t
    | Nope : unit t
end

module Current_state : sig
  type t [@@deriving sexp_of]

  val pc : t -> U16.t
  val reg : t -> [< Register.real ] -> U16.t
end

type t =
  | T :
      { current_io : 'a IO.t
      ; step : 'a -> t @ once
      }
      -> t

type create_result = Simulator of Current_state.t @@ many * t

val create : init_pc:U16.t -> create_result @ once
val drive : init_pc:U16.t -> f:('a. 'a IO.t -> ('a, 'res) Continue_or_stop.t) -> 'res
