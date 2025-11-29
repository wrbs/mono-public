open! Core
open! Async
module Jsonafable := Jsonaf.Jsonafable

module Params : sig
  type _ t =
    | No_params : unit t
    | Json : (module Jsonafable.S with type t = 'a) -> 'a t
end

module Method_call : sig
  type ('params, 'response) t =
    { method_ : string
    ; params : 'params Params.t
    ; response : (module Jsonafable.S with type t = 'response)
    }

  (** Creating *)

  val no_params
    :  string
    -> response:(module Jsonafable.S with type t = 'response)
    -> (unit, 'response) t

  val with_params
    :  string
    -> params:(module Jsonafable.S with type t = 'params)
    -> response:(module Jsonafable.S with type t = 'response)
    -> ('params, 'response) t

  (** Implementing *)

  val implement
    :  ('params, 'response) t
    -> ('state -> 'params -> 'response Deferred.t)
    -> 'state Handler.Implementation.t

  val implement_or_error
    :  ?error_code:int
    -> ('params, 'response) t
    -> ('state -> 'params -> 'response Or_error.t Deferred.t)
    -> 'state Handler.Implementation.t

  val implement_result
    :  ('params, 'response) t
    -> ('state -> 'params -> ('response, Jsonrpc.Rpc_error.t) Result.t Deferred.t)
    -> 'state Handler.Implementation.t

  (** Dispatching *)

  val to_call
    :  ('params, _) t
    -> 'params
    -> id:Jsonrpc.Id.t
    -> Jsonrpc.Request.Method_call.t

  val dispatch
    :  ('params, 'response) t
    -> Connection.t
    -> 'params
    -> 'response Or_error.t Deferred.t

  val dispatch_result
    :  ('params, 'response) t
    -> Connection.t
    -> 'params
    -> ('response, Jsonrpc.Rpc_error.t) Result.t Or_error.t Deferred.t

  module Full_error_kind : sig
    type t =
      | Rpc_error of Jsonrpc.Rpc_error.t
      | Problem_parsing_response of Error.t
      | Problem_receiving_response of Error.t
  end

  val dispatch_error_kind
    :  ('params, 'response) t
    -> Connection.t
    -> 'params
    -> ('response, Full_error_kind.t) Result.t Deferred.t Or_error.t Deferred.t
end

module Notification : sig
  type 'params t =
    { method_ : string
    ; params : 'params Params.t
    }

  (** Creating *)

  val no_params : string -> unit t

  val with_params
    :  string
    -> params:(module Jsonafable.S with type t = 'params)
    -> 'params t

  (** Implementing *)

  val implement
    :  'params t
    -> ('state -> 'params -> unit Deferred.t)
    -> 'state Handler.Implementation.t

  (** Dispatching *)

  val to_call : 'params t -> 'params -> Jsonrpc.Request.Notification.t
  val dispatch : 'params t -> Connection.t -> 'params -> unit Or_error.t Deferred.t
end
