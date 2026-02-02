open! Core
open! Hardcaml
open! Signal
(*
   module I = struct
  type 'a t =
    { addr : 'a [@bits 16]
    ; data_in : 'a [@bits 8]
    ; enable : 'a [@bits 8]
    ; rw : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

module Driver = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; mask : 'a [@bits 8]
    }
  [@@deriving hardcaml]
end

type t =
  { cpu_bus : Signal.t I.t
  ; mutable drivers : Signal.t Driver.t list
  ; scope : Scope.t
  }

let create scope =
  let%hw.I.Of_signal cpu_bus = I.Of_signal.wires () in
  { cpu_bus; drivers = []; scope }
;;

let add t driver = t.drivers <- driver :: t.drivers
let inputs t = t.cpu_bus
let complete t ~addr ~enable ~rw = () *)
