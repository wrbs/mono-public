open! Core
open! Hardcaml

val mtu : int
val max_data_bytes : int
val max_data_bits : int
val udp_protocol : int
val tcp_protocol : int
val ipv4_ethertype : int
val preamble_sfd_bits : int
val fcs_bits : int
val ifg_bits : int

module Ethernet : sig
  type 'a t =
    { dst_mac : 'a
    ; src_mac : 'a
    ; ethertype : 'a
    }
  [@@deriving hardcaml]
end

module Ipv4 : sig
  type 'a t =
    { version : 'a
    ; ihl : 'a
    ; dscp : 'a
    ; ecn : 'a
    ; length : 'a
    ; identification : 'a
    ; flags : 'a
    ; fragment_offset : 'a
    ; ttl : 'a
    ; protocol : 'a
    ; checksum : 'a
    ; src_ip : 'a
    ; dst_ip : 'a
    }
  [@@deriving hardcaml]
end

module Udp : sig
  type 'a t =
    { src_port : 'a
    ; dst_port : 'a
    ; length : 'a
    ; checksum : 'a
    }
  [@@deriving hardcaml]
end
