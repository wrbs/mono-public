open! Core
open! Hardcaml
open! Signal

module Roms : sig
  val prg_bits : int
  val chr_bits : int

  module I : sig
    type 'a t =
      { prg_addr : 'a
      ; prg_enable : 'a
      ; chr_addr : 'a
      ; chr_enable : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { prg : 'a
      ; chr : 'a
      }
    [@@deriving hardcaml]
  end
end

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; cartridge_in : 'a
    ; header : 'a array
    ; roms_out : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { header_valid : 'a
    ; cartridge_out : 'a Roms.O.t
    ; roms_in : 'a Roms.I.t
    }
  [@@deriving hardcaml]
end
