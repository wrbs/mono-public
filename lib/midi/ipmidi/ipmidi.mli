open! Core
open! Async

(** A library for the protocol https://nerds.de/en/ipmidi.html understands.

    The 'protocol' is basically non-existant: send raw midi over UDP (just the payload, no
    wrappers) to a multicast address/port.

    Adddress: [225.0.0.37] Port: [21928-21947] (20 ports supported)

    If the packet's dropped, tough. You've missed some messages. *)

module Port : sig @@ portable
  type t =
    | Port1
    | Port2
    | Port3
    | Port4
    | Port5
    | Port6
    | Port7
    | Port8
    | Port9
    | Port10
    | Port11
    | Port12
    | Port13
    | Port14
    | Port15
    | Port16
    | Port17
    | Port18
    | Port19
    | Port20
  [@@deriving enumerate, sexp_of, compare ~localize, equal ~localize]
end

module Port_sender : sig
  type t

  val create
    :  port:Port.t
    -> send_sync:
         (Async.Fd.t
          -> (Core.read, Iobuf.seek, Iobuf.global) Iobuf.t
          -> Core_unix.Syscall_result.Unit.t)
    -> t Async.Deferred.t

  val flush_exn : t -> unit
  val write_exn : t -> Midi.Live_message.t -> unit
  val close : t @ unique -> unit
end

module Sender : sig
  type t

  val create : unit -> t Async.Deferred.t
  val write_exn : t -> Midi.Live_message.t -> port:Port.t -> unit
  val write_and_flush_exn : t -> Midi.Live_message.t -> port:Port.t -> unit
  val flush_exn : t -> unit
  val flush_port_exn : t -> port:Port.t -> unit
  val close : t @ unique -> unit
end
