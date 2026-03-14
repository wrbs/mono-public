open! Hardcaml_home

module I : sig
  type 'a t =
    { clocking : 'a Clocking.t
    ; ready : 'a
    ; tx_a : 'a Packet_stream.t
    ; tx_b : 'a Packet_stream.t
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { tx : 'a Packet_stream.t
    ; ready_a : 'a
    ; ready_b : 'a
    ; b_emitting : 'a
    }
  [@@deriving hardcaml]
end

include functor Component.Make_S
