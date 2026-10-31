open Core
module Register_interface = Hardcaml_paper_demo_rtl.Register_interface
module Precision = Hardcaml_computation_dsl.Precision

module Make (Hardware_io : Hardware_io.S) = struct
  type t =
    { hardware_io : Hardware_io.t
    ; convolution_window_size : int
    ; dma_buffer : Bigstring.t
    ; element_num_bytes : int
    ; element_precision : Precision.t
    }

  let read_addresses = Register_interface.Rd.byte_addresses
  let write_addresses = Register_interface.Wr.byte_addresses
  let dma_buffer_num_bytes = 2048

  let read_precision_from_registers (h @ local) hardware_io =
    let f_bits =
      Hardware_io.read_register h hardware_io ~byte_address:read_addresses.element_f_bits
    in
    let i_bits =
      Hardware_io.read_register h hardware_io ~byte_address:read_addresses.element_i_bits
    in
    let signed =
      Hardware_io.read_register h hardware_io ~byte_address:read_addresses.element_signed
      = 1
    in
    { Precision.f_bits; i_bits; signed }
  ;;

  (* For simplicity in the paper demo, each element occupies one 32-bit word in the AXI
     stream, regardless of the actual element precision. *)
  let stream_word_num_bytes = 4

  let create (h @ local) hardware_io =
    let convolution_window_size =
      Hardware_io.read_register h hardware_io ~byte_address:read_addresses.window_size
    in
    let dma_buffer = Bigstring.create dma_buffer_num_bytes in
    let element_precision = read_precision_from_registers h hardware_io in
    let element_num_bytes =
      let element_num_bits = Precision.width element_precision in
      assert (element_num_bits % 8 = 0);
      element_num_bits / 8
    in
    { hardware_io
    ; convolution_window_size
    ; dma_buffer
    ; element_precision
    ; element_num_bytes
    }
  ;;

  let reset_kernel_window (h @ local) (t : t) ~(kernel : float# array) =
    let open Result.Let_syntax in
    let%bindl_fun () =
      if Array.length kernel <> t.convolution_window_size
      then
        Or_error.error_s
          [%message
            "The provided kernel's size does not match those of the hardware convolution \
             window!"
              (t.convolution_window_size : int)]
      else Ok ()
    in
    let%bindl_fun kernel_in_fixed_point =
      try
        Ok
          ((Array.map [@kind float64 value_or_null]) kernel ~f:(fun weight ->
             Fixed_point_utils.float_to_fixed_int t.element_precision weight))
      with
      | exn -> Or_error.of_exn exn
    in
    Hardware_io.write_register
      h
      t.hardware_io
      ~byte_address:write_addresses.clear_sliding_window
      1;
    Array.iter kernel_in_fixed_point ~f:(fun weight ->
      Hardware_io.write_register
        h
        t.hardware_io
        ~byte_address:write_addresses.wr_kernel_value
        weight);
    Ok ()
  ;;

  let convolution_window_size t = t.convolution_window_size

  let copy_inputs_to_dma_buffer (t : t) (input_elements : float# array) =
    (Array.iteri [@mode float64]) input_elements ~f:(fun i input_element ->
      let input_element_int =
        Fixed_point_utils.float_to_fixed_int t.element_precision input_element
      in
      (* Each element occupies one 32-bit word in the stream *)
      Bigstring.set_int32_le_exn
        t.dma_buffer
        ~pos:(i * stream_word_num_bytes)
        input_element_int);
    stream_word_num_bytes * Array.length input_elements
  ;;

  let copy_outputs_from_dma_buffer
    (t : t)
    (output_elements : float# array)
    ~output_num_elements
    : unit
    =
    for i = 0 to output_num_elements - 1 do
      (* Each element occupies one 32-bit word in the stream (sign-extended) *)
      let output_element_int =
        Bigstring.get_int32_le t.dma_buffer ~pos:(i * stream_word_num_bytes)
      in
      let output_element =
        Fixed_point_utils.fixed_int_to_float t.element_precision output_element_int
      in
      output_elements.(i) <- output_element
    done
  ;;

  let write_inputs_and_read_outputs
    (h @ local)
    (t : t)
    ~(src : float# array)
    ~(dst : float# array)
    : unit
    =
    if Array.length src > Array.length dst
    then (
      let src_num_elements = Array.length src in
      let dst_num_elements = Array.length dst in
      raise_s
        [%message
          "There is a mismatch between the number of elements in the source and dst"
            (src_num_elements : int)
            (dst_num_elements : int)]);
    let host_to_fpga_packet_num_bytes = copy_inputs_to_dma_buffer t src in
    Hardware_io.host_to_fpga
      h
      t.hardware_io
      t.dma_buffer
      ~len:host_to_fpga_packet_num_bytes;
    let fpga_to_host_packet_num_bytes =
      Hardware_io.fpga_to_host h t.hardware_io t.dma_buffer
    in
    let output_num_elements =
      assert (fpga_to_host_packet_num_bytes > 0);
      assert (fpga_to_host_packet_num_bytes % stream_word_num_bytes = 0);
      fpga_to_host_packet_num_bytes / stream_word_num_bytes
    in
    copy_outputs_from_dma_buffer t dst ~output_num_elements
  ;;
end
