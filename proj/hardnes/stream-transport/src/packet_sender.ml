open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; downstream : 'a Byte_stream.Tx.t
    ; flush : 'a
    ; conf : 'a Network_conf.t
    ; partial_checksum : 'a
    ; host_mac : 'a [@width mac_bits]
    ; configuration_valid : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; uart : 'a Byte_stream.Rx.t
    ; init_partial_checksum : 'a
    }
  [@@deriving hardcaml]
end
