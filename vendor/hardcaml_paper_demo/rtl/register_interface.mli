(** Register interface to interact with the top-level design. *)

module Rd : sig
  type 'a t =
    { num_input_elements_processed : 'a
    ; window_size : 'a
    ; element_signed : 'a
    ; element_i_bits : 'a
    ; element_f_bits : 'a
    }
  [@@deriving hardcaml]

  val byte_addresses : int t
end

module Wr : sig
  type 'a t =
    { wr_kernel_value : 'a
    ; clear_sliding_window : 'a
    }
  [@@deriving hardcaml]

  val byte_addresses : int t
end
