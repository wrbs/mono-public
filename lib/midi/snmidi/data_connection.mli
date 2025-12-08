open! Core
open! Async

type t

val create : Socket.Address.Inet.t -> on_send:(Protocol.Seqnum.t -> unit) -> t Deferred.t
val send_now : t -> Midi.Live_message.t Collection.t -> reset:bool -> unit

val send_queue
  :  t
  -> (delta_ms:int * Midi.Live_message.t Collection.t) Collection.t
  -> unit

val shutdown : t -> unit
