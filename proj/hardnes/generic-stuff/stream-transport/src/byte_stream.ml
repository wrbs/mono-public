open! Core
open! Hardcaml

module Rx = struct
  type 'a t =
    { rx_data : 'a [@bits 8]
    ; rx_valid : 'a
    ; error : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module Tx = struct
  type 'a t =
    { tx_data : 'a [@bits 8]
    ; tx_valid : 'a
    }
  [@@deriving hardcaml]
end
