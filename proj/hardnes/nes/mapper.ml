open! Core
open! Hardcaml
open! Signal

module Roms = struct
  let prg_bits = 18
  let chr_bits = 17

  module I = struct
    type 'a t =
      { prg_addr : 'a [@bits prg_bits]
      ; prg_enable : 'a
      ; chr_addr : 'a [@bits chr_bits]
      ; chr_enable : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { prg : 'a [@bits 8]
      ; chr : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end
end

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; cartridge_in : 'a
    ; header : ('a[@bits 8]) array [@length 16]
    ; roms_out : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { header_valid : 'a
    ; cartridge_out : 'a Roms.O.t
    ; roms_in : 'a Roms.I.t
    }
  [@@deriving hardcaml]
end
