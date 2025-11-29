open! Core
open! Async

(** A transport based on an underlying (reader, writer) stream using line-delimited
    messages.

    Messages must be separated by newlines (LF) and cannot contain newlines themselves. *)

val create
  :  ?batch_how:Monad_sequence.how
  -> ?implementations:'state Handler.Implementations.t
  -> ?on_close:(reason:Error.t option -> unit Deferred.t)
  -> Reader.t
  -> Writer.t
  -> connection_state:(Connection.t -> 'state)
  -> Connection.t

module Stdio : sig
  (** A command that implements a stdio streaming lines JSON-RPC *)
  val command
    :  ?batch_how:Monad_sequence.how
    -> ?readme:(unit -> string)
    -> summary:string
    -> implementations:'conn_state Handler.Implementations.t
    -> connection_state:('state -> Connection.t -> 'conn_state)
    -> 'state Or_error.t Deferred.t Command.Param.t
    -> Command.t

  (** A more general version of [command] allowing the implementations to depend on the
      params *)
  val command'
    :  ?batch_how:Monad_sequence.how
    -> ?readme:(unit -> string)
    -> summary:string
    -> (unit -> Connection.t Handler.Implementations.t Or_error.t Deferred.t)
         Command.Param.t
    -> Command.t
end
