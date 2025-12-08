open! Core
open! Async

module Problem : sig
  module Server : sig
    type t [@@deriving sexp_of]

    val pretty_ansi_format : t -> string
    val to_error : t -> Error.t
  end

  type t =
    | Server of Server.t
    | Client of Error.t
  [@@deriving sexp_of]

  val to_error : t -> Error.t
  val to_or_error : ('a, t) result -> 'a Or_error.t
end

module Port : sig
  (** A midi port *)

  module Id : sig
    type t = private string [@@deriving jsonaf]

    include String_id.S with type t := t
  end

  type t =
    { id : Id.t
    ; name : string
    }
  [@@deriving jsonaf, sexp_of]
end

(** A client *)

type t

(** Connection automatically shutdown when deferred resolves *)

module Close_reason : sig
  type t =
    | Shutdown_requested
    | Problem of Problem.t
  [@@deriving sexp_of]
end

val list_ports
  :  ?connect_timeout:Time_ns.Span.t
  -> Socket.Address.Inet.t
  -> client_name:string
  -> (Port.t list, Problem.t) result Deferred.t

val with_
  :  ?connect_timeout:Time_ns.Span.t
  -> ?udp_ack_timeout:Time_ns.Span.t
  -> ?don't_stop_notes_on_shutdown:unit
  -> addr:Socket.Address.Inet.t
  -> client_name:string
  -> (Port.t list
      -> [ `Don't_connect of 'a | `Connect of Port.Id.t * (t -> 'a Deferred.t) ]
           Deferred.t)
  -> ('a, Problem.t) result Deferred.t

(** Sending messages *)

val send : t -> Midi.Live_message.t Collection.t -> (unit, Close_reason.t) Result.t

val reset_queue
  :  t
  -> cleanup:
       [ `Don't_cleanup | `Stop_all_notes | `Custom of Midi.Live_message.t Collection.t ]
  -> (unit, Close_reason.t) Result.t

val queue_messages
  :  t
  -> (delta_ms:int * Midi.Live_message.t Collection.t) Collection.t
  -> (unit, Close_reason.t) Result.t

(** These functions silently do nothing if the connection is closed *)

val send_if_connected : t -> Midi.Live_message.t Collection.t -> unit

val reset_queue_if_connected
  :  t
  -> cleanup:
       [ `Don't_cleanup | `Stop_all_notes | `Custom of Midi.Live_message.t Collection.t ]
  -> unit

val queue_messages_if_connected
  :  t
  -> (delta_ms:int * Midi.Live_message.t Collection.t) Collection.t
  -> unit

(** You can also avoid with if that makes things cleaner for you *)

val connect'
  :  ?connect_timeout:Time_ns.Span.t
  -> ?udp_ack_timeout:Time_ns.Span.t
  -> Socket.Address.Inet.t
  -> client_name:string
  -> pick_port:
       (Port.t list
        -> [ `Don't_connect of 'abort | `Connect of Port.Id.t * 'a ] Deferred.t)
  -> ([ `Connected of t * 'a | `Aborted of 'abort ], Problem.t) result Deferred.t

val connect
  :  ?connect_timeout:Time_ns.Span.t
  -> ?udp_ack_timeout:Time_ns.Span.t
  -> Socket.Address.Inet.t
  -> client_name:string
  -> pick_port:
       (Port.t list -> [ `Don't_connect of 'abort | `Connect of Port.Id.t ] Deferred.t)
  -> ([ `Connected of t | `Aborted of 'abort ], Problem.t) result Deferred.t

val connect_or_error
  :  ?connect_timeout:Time_ns.Span.t
  -> ?udp_ack_timeout:Time_ns.Span.t
  -> Socket.Address.Inet.t
  -> client_name:string
  -> pick_port:(Port.t list -> Port.Id.t Or_error.t Deferred.t)
  -> t Or_error.t Deferred.t

val is_connected : t -> bool
val shutdown_if_connected : t -> stop_all_notes:bool -> unit
val close_finished : t -> Close_reason.t Deferred.t
