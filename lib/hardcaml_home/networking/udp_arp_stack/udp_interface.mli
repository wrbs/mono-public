open! Core
open! Hardcaml_home
open! Hardcaml_home_networking

module In : sig
  type 'a t =
    { src_mac : 'a Addr.Mac.t
    ; src_ip : 'a Addr.Ip.t
    ; src_port : 'a [@bits 16]
    ; dst_port : 'a [@bits 16]
    ; payload_length : 'a [@bits 16]
    ; rx : 'a Packet_stream.t
    ; rx_valid : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module Out : sig
  type 'a t =
    { dst_mac : 'a Addr.Mac.t
    ; dst_ip : 'a Addr.Ip.t
    ; src_port : 'a
    ; dst_port : 'a
    ; payload_length : 'a
    ; tx : 'a Packet_stream.t
    }
  [@@deriving hardcaml]
end
