open! Core
open! Hardcaml
open! Signal

module PPU = struct
  let dot_bits = num_bits_to_represent 340
  let scanline_bits = num_bits_to_represent 261

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; tick : 'a
      ; cpu_addr_3 : 'a [@bits 3]
      ; cart_data : 'a [@bits 8]
      ; cpu_addr_in_range : 'a
      ; ppu_data_in : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ppu_addr : 'a [@bits 14]
      ; ppu_data : 'a [@bits 8]
      ; ppu_read : 'a
      ; ppu_write : 'a
      ; nmi : 'a
      ; pixel : 'a [@bits 6]
      ; output : 'a
          (* high when pixel has been output *)
          (* For debugging: *)
      ; frame : 'a (* high on last cycle of frame where there's [output] *)
      ; dot : 'a [@bits dot_bits]
      ; scanline : 'a [@bits scanline_bits]
      }
    [@@deriving hardcaml]
  end
end

module Nes = struct
  module O = struct
    type 'a t =
      { state : 'a
      ; fetching : 'a (* Cpu debug *)
      ; pc : 'a
      ; a : 'a
      ; s : 'a
      ; x : 'a
      ; y : 'a
      ; p : 'a
      ; illegal_instruction : 'a
      }
    [@@deriving hardcaml]
  end
end
