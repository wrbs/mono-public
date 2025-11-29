open! Core
open! Async

(** A connection is responsible for

    - tracking the lifecycle of a connection
    - sending requests and handling responses *)

type t

(** Triggers the connections to close and waits for it to finish.

    [reason] is used as the reason if it's the first call to [close]. Subsequent calls
    ignore [reason] and instead behave the same as [close_finished]. *)
val close : ?reason:Error.t -> t -> unit Deferred.t

(** Waits for the connection to close, without triggering it *)
val close_finished : t -> unit Deferred.t

module Untyped : sig
  (** Sends a notification. Returns an error if there's errors sending. *)
  val send_notification : ?params:Jsonaf.t -> t -> string -> unit Or_error.t Deferred.t

  (** Calls a method. All errors are combined into one [Error] *)
  val call_method : ?params:Jsonaf.t -> t -> string -> Jsonaf.t Or_error.t Deferred.t

  (** Calls a method. Returns an outer [Error] if there's an error sending the message, or
      receiving the response. The inner [Result.t] returns the server response. *)
  val call_method'
    :  ?params:Jsonaf.t
    -> t
    -> string
    -> (Jsonaf.t, Jsonrpc.Rpc_error.t) Result.t Or_error.t Deferred.t

  (** Calls a method. Distinguishes error sending the request (outer [Or_error]),
      receiving the response (middle [Or_error]) and error responses from the server
      (innermost [Result]) *)
  val call_method''
    :  ?params:Jsonaf.t
    -> t
    -> string
    -> (Jsonaf.t, Jsonrpc.Rpc_error.t) Result.t Or_error.t Deferred.t Or_error.t
         Deferred.t
end

module Expert : sig
  (** Creates a client, using [send_request] to send requests *)
  val create
    :  send_request:(Jsonrpc.Request.t -> unit Or_error.t Deferred.t)
    -> on_close:(reason:Error.t option -> unit Deferred.t)
    -> t * [ `handle_response of Jsonrpc.Response.t -> unit Or_error.t ]
end
