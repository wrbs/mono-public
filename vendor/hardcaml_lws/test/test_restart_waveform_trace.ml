open Core
open Hardcaml
open Hardcaml_lws
module Dut = Single_clock_domain_counter
module L = Lws_cyclesim.With_interface (Dut.I) (Dut.O)

type sim_context = L.sim_context

let backend_specific_config =
  { Lws_cyclesim.Backend_specific_config.default with waves = true }
;;

let%expect_test "waveform_trace `on starts a fresh trace, `off disables tracing" =
  let lws = L.create ~backend_specific_config Dut.create in
  Lws_cyclesim.schedule_task lws (fun h (sim_context : sim_context) ->
    sim_context.inputs.enable := Bits.vdd;
    for _ = 1 to 3 do
      Lws.step h
    done)
  |> Lws_cyclesim.poll_task lws;
  (* Print the first waveform trace. *)
  Lws_context.Waveform.expect ~show_digest:false ~wave_width:4 (Lws_cyclesim.context lws);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌│
    │               ││     └────┘    └────┘    └────┘    └────┘    └────┘│
    │enable         ││────────────────────────────────────────           │
    │               ││                                                   │
    │               ││──────────┬─────────┬─────────┬─────────           │
    │count          ││ 00000000 │00000001 │00000002 │00000003            │
    │               ││──────────┴─────────┴─────────┴─────────           │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  (* Turn tracing off. *)
  Lws_cyclesim.waveform_trace lws `off;
  (* Verify waveform is now None. *)
  let waveform_is_none =
    Lws_context.waveform (Lws_cyclesim.context lws) |> Option.is_none
  in
  print_s [%message (waveform_is_none : bool)];
  [%expect {| (waveform_is_none true) |}];
  (* Run some cycles while tracing is off. *)
  Lws_cyclesim.schedule_task lws (fun h (_ : sim_context) ->
    for _ = 1 to 2 do
      Lws.step h
    done)
  |> Lws_cyclesim.poll_task lws;
  (* Turn tracing back on and run more cycles. *)
  Lws_cyclesim.waveform_trace lws `on;
  Lws_cyclesim.schedule_task lws (fun h (_ : sim_context) ->
    for _ = 1 to 2 do
      Lws.step h
    done)
  |> Lws_cyclesim.poll_task lws;
  (* Print the second waveform trace - should only contain cycles after `on. *)
  Lws_context.Waveform.expect ~show_digest:false ~wave_width:4 (Lws_cyclesim.context lws);
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌────┐    ┌│
    │               ││     └────┘    └────┘    └────┘    └────┘    └────┘│
    │enable         ││──────────────────────────────                     │
    │               ││                                                   │
    │               ││──────────┬─────────┬─────────                     │
    │count          ││ 00000007 │00000008 │00000009                      │
    │               ││──────────┴─────────┴─────────                     │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
