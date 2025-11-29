open! Core
open! Async

module Implementation : sig
  type 'a t

  val method_call
    :  method_:string
    -> parse_params:(Jsonaf.t option -> ('params, Jsonaf.t option) Result.t)
    -> ('state -> 'params -> (Jsonaf.t, Jsonrpc.Rpc_error.t) Result.t Deferred.t)
    -> 'state t

  val notification
    :  method_:string
    -> parse_params:(Jsonaf.t option -> 'params option)
    -> ('state -> 'params -> unit Deferred.t)
    -> 'state t

  val lift : 'b t -> f:('a -> 'b) -> 'a t
end

module Implementations : sig
  type 'a t

  val create : 'a Implementation.t list -> 'a t Or_error.t
  val create_exn : 'a Implementation.t list -> 'a t
  val lift : 'b t -> f:('a -> 'b) -> 'a t
  val with_state : 'a t -> 'a -> unit t
end

module Single : sig
  val handle_call
    :  Jsonrpc.Request.Call.t
    -> implementations:unit Implementations.t
    -> Jsonrpc.Response.Output.t option Deferred.t

  val handle_request
    :  ?batch_how:Monad_sequence.how
    -> Jsonrpc.Request.t
    -> implementations:unit Implementations.t
    -> Jsonrpc.Response.t option Deferred.t

  val handle_request'
    :  ?batch_how:Monad_sequence.how
    -> (Jsonrpc.Request.t, [ `bad_json ]) Result.t
    -> implementations:unit Implementations.t
    -> Jsonrpc.Response.t option Deferred.t

  val handle_string
    :  ?batch_how:Monad_sequence.how
    -> string
    -> implementations:unit Implementations.t
    -> string Deferred.t
end

module Multi : sig
  val handle_requests
    :  ?how:Monad_sequence.how
    -> ?batch_how:Monad_sequence.how
    -> Jsonrpc.Request.t list
    -> implementations:unit Implementations.t
    -> Jsonrpc.Response.t list Deferred.t

  val handle_string
    :  ?how:Monad_sequence.how
    -> ?batch_how:Monad_sequence.how
    -> string
    -> implementations:unit Implementations.t
    -> string Deferred.t
end
