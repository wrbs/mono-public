open! Core
open! Hardcaml
(*
   module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; cpu : 'a Cpu_bus.I.t
    ; ppu_addr : 'a [@bits 14]
    ; ppu_data_in : 'a [@bits 8]
    ; ppu_read : 'a
    ; ppu_write : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { cpu : 'a Cpu_bus.O.t
    ; ppu_data : 'a [@bits 8] (* todo: irq *)
    ; irq : 'a
    }
end *)
