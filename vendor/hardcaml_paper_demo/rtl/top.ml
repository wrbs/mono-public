open! Core
open! Hardcaml
open! Signal
module Internal_bus = Hardcaml_axi.Internal_bus.W32
module Register_mode = Hardcaml_axi.Register_mode

module Stream = Hardcaml_axi.Stream.Make (struct
    let data_bits = 32
    let user_bits = 1
  end)

module Register_bank =
  Internal_bus.Register_bank.With_interface
    (Register_interface.Rd)
    (Register_interface.Wr)

module Make (Config : Config.S) = struct
  module Conv = Conv1d.Make (Config)

  (* For simplicity in the paper demo, we use a fixed 32-bit AXI stream where each element
     occupies one 32-bit word (sign-extended on output, truncated on input), rather than
     packing elements tightly. *)

  (* In the [I] and [O] interface, we explicitly annotate [@rtlprefix] on
     fpga_to_host_dest and so that the waveform group them together with the other
     "fpga_to_host" signals and disable ~rtlmangle on the entire interface.

     ~rtlmangle:true, which is the default, places a "$" sign between the parent field
     names and the child field name, eg: making "host_to_fpga" and "tvalid" into
     "host_to_fpga$tvalid".
  *)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; host_to_fpga : 'a Stream.Source.t [@rtlmangle "$"]
      ; fpga_to_host_dest : 'a Stream.Dest.t [@rtlprefix "fpga_to_host$"]
      ; reg_master : 'a Internal_bus.Master_to_slave.t [@rtlmangle "$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module O = struct
    type 'a t =
      { host_to_fpga_dest : 'a Stream.Dest.t [@rtlprefix "host_to_fpga$"]
      ; fpga_to_host : 'a Stream.Source.t [@rtlmangle "$"]
      ; reg_slave : 'a Internal_bus.Slave_to_master.t [@rtlmangle "$"]
      }
    [@@deriving hardcaml ~rtlmangle:false]
  end

  module Fifo_data = struct
    type 'a t =
      { element : 'a [@bits Conv.element_width]
      ; is_last : 'a
      }
    [@@deriving hardcaml]
  end

  let create_weights_shreg ~clocking (wr_kernel_value : _ With_valid.t) =
    let regs = Vec.create () in
    for _ = 1 to Config.window_size do
      let back =
        match Vec.peek_back regs with
        | Null -> wr_kernel_value.value
        | This x -> x
      in
      Vec.push_back regs (Clocking.reg ~enable:wr_kernel_value.valid clocking back)
    done;
    List.rev (Vec.to_list regs)
  ;;

  let create ~build_mode (scope : Scope.t) (i : Signal.t I.t) =
    assert (Conv.element_width <= 32);
    let spec = Clocking.to_spec i.clocking in
    let%tydi { clocking; host_to_fpga; fpga_to_host_dest; reg_master } = i in
    let%tydi { clock; clear } = clocking in
    let%hw host_to_fpga_ready = wire 1 in
    (* Extract element from lower bits of 32-bit stream word *)
    let input_element_value = sel_bottom host_to_fpga.tdata ~width:Conv.element_width in
    let num_input_elements_processed =
      Clocking.reg_fb
        clocking
        ~width:32
        ~f:(fun fb -> fb +:. 1)
        ~enable:(host_to_fpga.tvalid &: host_to_fpga_ready)
    in
    let%tydi read_values =
      { Register_interface.Rd.num_input_elements_processed
      ; window_size = of_unsigned_int ~width:32 Config.window_size
      ; element_signed = of_bool Config.element_precision.signed
      ; element_i_bits = of_unsigned_int ~width:32 Config.element_precision.i_bits
      ; element_f_bits = of_unsigned_int ~width:32 Config.element_precision.f_bits
      }
    in
    let register_bank =
      Register_bank.hierarchical
        scope
        ~write_modes:
          { wr_kernel_value = Register_mode.hold
          ; clear_sliding_window = Register_mode.toggle_low
          }
        { clock; clear; clear_write_values = gnd; master = reg_master; read_values }
    in
    let weights =
      let wr_kernel_value =
        { register_bank.write_values.wr_kernel_value with
          value =
            sel_bottom
              register_bank.write_values.wr_kernel_value.value
              ~width:Conv.element_width
        }
      in
      create_weights_shreg ~clocking wr_kernel_value
    in
    let%tydi { output_element } =
      Conv.hierarchical
        ~build_mode
        scope
        { clocking
        ; kernel = { weights }
        ; input_element =
            { valid = host_to_fpga.tvalid &: host_to_fpga_ready
            ; value = input_element_value
            }
        ; clear_sliding_window = register_bank.write_values.clear_sliding_window.value
        }
    in
    let fifo_rd_enable = wire 1 in
    let output_fifo =
      let scope = Scope.sub_scope scope "output_fifo" in
      let%hw is_last =
        pipeline spec ~n:Conv.latency (i.host_to_fpga.tlast &: i.host_to_fpga.tvalid)
      in
      let d = Fifo_data.Of_signal.pack { is_last; element = output_element.value } in
      Fifo.create
        ~scope
        ~showahead:true
        ~capacity:(Conv.latency + 2)
        ~clock
        ~clear
        ~wr:output_element.valid
        ~d
        ~rd:fifo_rd_enable
        ()
    in
    let output_fifo_q = Fifo_data.Of_signal.unpack output_fifo.q in
    fifo_rd_enable <-- (fpga_to_host_dest.tready &: ~:(output_fifo.empty));
    host_to_fpga_ready <-- ~:(output_fifo.full);
    (* Sign-extend element to 32 bits for output stream *)
    let output_tdata = sresize output_fifo_q.element ~width:32 in
    { O.host_to_fpga_dest = { tready = host_to_fpga_ready }
    ; fpga_to_host =
        { tvalid = ~:(output_fifo.empty)
        ; tdata = output_tdata
        ; tkeep = ones 4
        ; tstrb = ones 4
        ; tlast = output_fifo_q.is_last
        ; tuser = zero 1
        }
    ; reg_slave = register_bank.slave
    }
  ;;
end
