open! Core
open! Hardcaml

module Rx : sig
  type 'a t =
    { rx_data : 'a
    ; rx_valid : 'a
    ; error : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module Tx : sig
  type 'a t =
    { tx_data : 'a
    ; tx_valid : 'a
    }
  [@@deriving hardcaml]
end
