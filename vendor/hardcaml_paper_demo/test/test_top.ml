open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_lws
open Unboxed
module Top = Hardcaml_paper_demo_rtl.Top
module Config = Hardcaml_paper_demo_rtl.Config
module Precision = Hardcaml_computation_dsl.Precision

let random = Splittable_random.create Random.State.default

module Make (Config : Config.S) = struct
  module Dut = Top.Make (Config)
  module L = Hardcaml_test_harness.Lws_harness.Make (Dut.I) (Dut.O)

  type sim_context = L.sim_context

  (* [Simulation_hardware_io] the version of hardware_io backed by a simulated design. *)
  module Simulation_hardware_io = struct
    module Stream = Top.Stream
    module Stream_sim = Stream_simulator.Make (Stream)
    module Handler = Lws.Handler

    type t = sim_context

    let read_register (h @ local) (t : t) ~byte_address =
      let inputs = t.inputs in
      let outputs = t.outputs in
      inputs.reg_master.address := Bits.of_unsigned_int ~width:32 byte_address;
      inputs.reg_master.read_valid := Bits.vdd;
      inputs.reg_master.read_first := Bits.vdd;
      Lws.step h;
      while not (Bits.to_bool !(outputs.before_edge.reg_slave.read_ready)) do
        inputs.reg_master.read_first := Bits.gnd;
        Lws.step h
      done;
      let result = Bits.to_unsigned_int !(outputs.before_edge.reg_slave.read_data) in
      inputs.reg_master.read_valid := Bits.gnd;
      inputs.reg_master.read_first := Bits.gnd;
      result
    ;;

    let write_register (h @ local) (t : t) ~byte_address value =
      let inputs = t.inputs in
      let outputs = t.outputs in
      inputs.reg_master.address := Bits.of_unsigned_int ~width:32 byte_address;
      (* We require a mask here because the data user is trying to write can be negative.
         Negative values have a valid representation in two complements, so we land with
         the mask.
      *)
      inputs.reg_master.write_data
      := Bits.of_unsigned_int ~width:32 (value land 0xFFFF_FFFF);
      inputs.reg_master.write_byte_en := Bits.ones 4;
      inputs.reg_master.write_valid := Bits.vdd;
      inputs.reg_master.write_first := Bits.vdd;
      Lws.step h;
      while not (Bits.to_bool !(outputs.before_edge.reg_slave.write_ready)) do
        inputs.reg_master.write_first := Bits.gnd;
        Lws.step h
      done;
      inputs.reg_master.write_valid := Bits.gnd;
      inputs.reg_master.write_first := Bits.gnd
    ;;

    let host_to_fpga (h @ local) (t : t) (data : Bigstring.t @ local) ~len =
      let inputs = t.inputs in
      let outputs = t.outputs in
      let packet = Bigstring.To_string.sub data ~pos:0 ~len in
      Stream_sim.send_packet
        h
        ~source:inputs.host_to_fpga
        ~dest:(Before_and_after_edge.map outputs ~f:(fun o -> o.host_to_fpga_dest))
        packet
    ;;

    let fpga_to_host (h @ local) (t : t) (data : Bigstring.t @ local) =
      let inputs = t.inputs in
      let outputs = t.outputs in
      let received =
        Stream_sim.recv_packet
          h
          ~source:(Before_and_after_edge.map outputs ~f:(fun o -> o.fpga_to_host))
          ~dest:inputs.fpga_to_host_dest
      in
      let len = String.length received in
      Bigstring.From_string.blit ~src:received ~src_pos:0 ~dst:data ~dst_pos:0 ~len;
      len
    ;;
  end

  module Driver = Hardcaml_paper_demo_driver.Driver.Make (Simulation_hardware_io)

  let compute_expected ~kernel (inputs : float list) : float list =
    let kernel_list = Array.to_list kernel in
    Software_model.conv1d Config.element_precision ~kernel:kernel_list ~input:inputs
  ;;

  let print_waveform ~start_cycle_additional_offset waveform =
    let display_rules =
      let open Display_rule in
      [ port_name_is "clock" ~wave_format:Bit
      ; port_name_is "host_to_fpga$tvalid" ~wave_format:Bit
      ; port_name_is "host_to_fpga$tready" ~wave_format:Bit
      ; port_name_is "host_to_fpga$tdata" ~wave_format:Hex
      ; port_name_is "host_to_fpga$tlast" ~wave_format:Bit
      ; port_name_is "fpga_to_host$tvalid" ~wave_format:Bit
      ; port_name_is "fpga_to_host$tready" ~wave_format:Bit
      ; port_name_is "fpga_to_host$tdata" ~wave_format:Hex
      ; port_name_is "fpga_to_host$tlast" ~wave_format:Bit
      ]
    in
    Hardcaml_waveterm.Waveform.print
      ~start_cycle:((Config.window_size * 3) + start_cycle_additional_offset)
      ~signals_width:28
      ~wave_width:1
      ~display_width:90
      ~display_rules
      waveform
  ;;

  let run_testbench ~print_waves_start_cycle_offset testbench =
    let print_waves_after_test =
      match print_waves_start_cycle_offset with
      | None -> None
      | Some start_cycle_additional_offset ->
        Some (print_waveform ~start_cycle_additional_offset)
    in
    L.run_advanced
      ?print_waves_after_test
      ~trace:`All_named
      ~create:(Dut.create ~build_mode:Simulation)
      testbench
  ;;

  let generate_float = Hardcaml_paper_demo_driver.Fixed_point_utils.generate_float

  let test_single_request
    ?print_waves_start_cycle_offset
    ?kernel
    (input_values : float list)
    =
    let kernel =
      match kernel with
      | Some kernel -> kernel
      | None ->
        List.gen_with_length Config.window_size (generate_float Config.element_precision)
        |> Quickcheck.Generator.generate ~random ~size:1
        |> Array.of_list
    in
    let testbench (h @ local) (sim_context : sim_context) =
      let driver = Driver.create h sim_context in
      let kernel_array : float# array =
        (Array.init [@kind float64]) (Array.length kernel) ~f:(fun i ->
          F64.of_float kernel.(i))
      in
      (match Driver.reset_kernel_window h driver ~kernel:kernel_array with
       | Ok () -> ()
       | Error e -> raise_s [%message "Failed to reset kernel" (e : Error.t)]);
      let num_values = List.length input_values in
      let src : float# array =
        (Array.init [@kind float64]) num_values ~f:(fun i ->
          F64.of_float (List.nth_exn input_values i))
      in
      let dst : float# array =
        (Array.init [@kind float64]) num_values ~f:(fun _ -> #0.)
      in
      Driver.write_inputs_and_read_outputs h driver ~src ~dst;
      (* Run a few more cycles to capture full waveform *)
      for _ = 1 to 3 do
        Lws.step h
      done;
      (* Validate results - should be exact since software model matches hardware rounding *)
      let received_floats =
        List.init num_values ~f:(fun i -> F64.to_float (Array.unsafe_get dst i))
      in
      let expected_floats = compute_expected ~kernel input_values in
      (* Print kernel *)
      print_s [%message (kernel : float array)];
      (* Print input/output table *)
      let rows = List.zip_exn input_values received_floats in
      let columns =
        [ Ascii_table.Column.create ~align:Right "Input" (fun (input, _) ->
            sprintf "%.4f" input)
        ; Ascii_table.Column.create ~align:Right "Output" (fun (_, output) ->
            sprintf "%.4f" output)
        ]
      in
      print_endline (Ascii_table.to_string columns rows);
      if not ([%equal: float list] received_floats expected_floats)
      then
        raise_s
          [%message
            "Result mismatch!"
              (expected_floats : float list)
              (received_floats : float list)
              (input_values : float list)
              (kernel : float array)]
    in
    run_testbench ~print_waves_start_cycle_offset testbench
  ;;
end

module%test Test_with_size_3_window = struct
  include Make (struct
      let window_size = 3
      let element_precision = { Precision.signed = true; i_bits = 8; f_bits = 8 }
    end)

  let%expect_test "3 values with waveform" =
    test_single_request [ 0.125; 0.375; -0.25 ];
    [%expect
      {|
      (kernel (0 -2.90625 124.5390625))
      ┌─────────┬──────────┐
      │   Input │   Output │
      ├─────────┼──────────┤
      │  0.1250 │  15.5664 │
      │  0.3750 │  46.3359 │
      │ -0.2500 │ -32.2266 │
      └─────────┴──────────┘
      |}]
  ;;

  let%expect_test "5 values with waveform" =
    test_single_request
      ~print_waves_start_cycle_offset:5
        (* Start from cycle 5 to show the more interesting segment of the waveform. *)
      [ 1.0; 2.0; 3.0; 4.0; 5.0 ];
    [%expect
      {|
      (kernel (0.140625 -22.234375 3.07421875))
      ┌────────┬──────────┐
      │  Input │   Output │
      ├────────┼──────────┤
      │ 1.0000 │   3.0742 │
      │ 2.0000 │ -16.0859 │
      │ 3.0000 │ -35.1055 │
      │ 4.0000 │ -54.1250 │
      │ 5.0000 │ -73.1445 │
      └────────┴──────────┘

      ┌Signals───────────────────┐┌Waves───────────────────────────────────────────────────────┐
      │clock                     ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
      │                          ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
      │host_to_fpga$tvalid       ││        ┌───────────────────┐                               │
      │                          ││────────┘                   └───────────────────────────────│
      │host_to_fpga$tready       ││────────────────────────────────────────────────────────────│
      │                          ││                                                            │
      │                          ││────────┬───┬───┬───┬───┬───────────────────────────────────│
      │host_to_fpga$tdata        ││ 000000.│00.│00.│00.│00.│00000500                           │
      │                          ││────────┴───┴───┴───┴───┴───────────────────────────────────│
      │host_to_fpga$tlast        ││                        ┌───────────────────────────────────│
      │                          ││────────────────────────┘                                   │
      │fpga_to_host$tvalid       ││                            ┌───────────────────────────┐   │
      │                          ││────────────────────────────┘                           └───│
      │fpga_to_host$tready       ││                                ┌───┐   ┌───────────────┐   │
      │                          ││────────────────────────────────┘   └───┘               └───│
      │                          ││────────────────────────────┬───────┬───────┬───┬───┬───┬───│
      │fpga_to_host$tdata        ││ 00000000                   │000003.│FFFFEF.│FF.│FF.│FF.│00.│
      │                          ││────────────────────────────┴───────┴───────┴───┴───┴───┴───│
      │fpga_to_host$tlast        ││                                                    ┌───┐   │
      │                          ││────────────────────────────────────────────────────┘   └───│
      └──────────────────────────┘└────────────────────────────────────────────────────────────┘
      |}]
  ;;
end

module%test Test_with_size_5_window = struct
  module Config = struct
    let window_size = 5
    let element_precision = { Precision.signed = true; i_bits = 8; f_bits = 8 }
  end

  include Make (Config)

  let%expect_test "10 values" =
    let values =
      Quickcheck.Generator.generate
        ~size:1
        ~random
        (List.gen_with_length 5 (generate_float Config.element_precision))
    in
    test_single_request values;
    [%expect
      {|
      (kernel (-0.234375 53.19921875 -128 -1.296875 0.08203125))
      ┌──────────┬───────────┐
      │    Input │    Output │
      ├──────────┼───────────┤
      │  -0.0039 │   -0.0039 │
      │   1.2266 │    0.1055 │
      │  -5.7500 │   -1.5625 │
      │ 127.9961 │ -128.0000 │
      │   5.4453 │  127.9961 │
      └──────────┴───────────┘
      |}]
  ;;
end
