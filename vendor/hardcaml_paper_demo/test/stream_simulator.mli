open Hardcaml
open Hardcaml_lws

module Make (Stream : Hardcaml_axi.Stream.S) : sig
  open Stream

  val send_packet
    :  Lws.Handler.t @ local
    -> source:Bits.t ref Source.t
    -> dest:Bits.t ref Dest.t Before_and_after_edge.t
    -> string
    -> unit

  val recv_packet
    :  Lws.Handler.t @ local
    -> source:Bits.t ref Source.t Before_and_after_edge.t
    -> dest:Bits.t ref Dest.t
    -> string
end
