open! Core
open! Async

type t

val create : Reader.t -> Writer.t -> t
val shutdown : t -> unit
val read_opt : t -> (Jsonaf.t -> 'a) -> ('a option, Protocol.Problem.t) result Deferred.t
val write_exn : t -> ('a -> Jsonaf.t) -> 'a -> unit

val call
  :  (module Protocol.Call with type Request.t = 'request and type Response.t = 'response)
  -> ?timeout:Time_ns.Span.t
  -> t
  -> 'request
  -> ('response, Protocol.Problem.t) result Deferred.t
