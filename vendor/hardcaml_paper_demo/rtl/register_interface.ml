module Rd = struct
  type 'a t =
    { num_input_elements_processed : 'a [@bits 32]
    ; window_size : 'a [@bits 32]
    ; element_signed : 'a [@bits 1]
    ; element_i_bits : 'a [@bits 32]
    ; element_f_bits : 'a [@bits 32]
    }
  [@@deriving hardcaml]

  let byte_addresses = scan ~init:0 port_names ~f:(fun acc _ -> acc + 4, acc)
end

module Wr = struct
  type 'a t =
    { wr_kernel_value : 'a [@bits 32]
    ; clear_sliding_window : 'a
    }
  [@@deriving hardcaml]

  let byte_addresses = scan ~init:0 port_names ~f:(fun acc _ -> acc + 4, acc)
end
