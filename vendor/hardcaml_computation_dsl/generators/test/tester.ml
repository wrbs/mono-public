open Core
open Hardcaml
open Hardcaml_waveterm
open Bits
open Hardcaml_computation_dsl
include Tester_intf
module Fixed_signed = Hardcaml_fixed_point.Signed (Bits)
module Fixed_unsigned = Hardcaml_fixed_point.Unsigned (Bits)

module Make (To_test : To_test) = struct
  open To_test
  module Sim = Cyclesim.With_interface (I) (O)
  module Step = Hardcaml_step_testbench.Functional.Cyclesim.Make (I) (O)

  let create_sim ~waves () =
    let scope =
      Scope.create ~flatten_design:true ~auto_label_hierarchical_ports:true ()
    in
    let sim =
      Sim.create
        ~config:Cyclesim.Config.trace_all
        (hierarchical ~build_mode:Simulation scope)
    in
    Cyclesim.Waveform.create_if ~enabled:waves sim
  ;;

  (* Post-clear cycle counter. *)
  let cycle_counter = ref 0

  let create_and_clear_sim ~waves () =
    cycle_counter := 0;
    let waves, sim = create_sim ~waves () in
    let inputs : _ I.t = Cyclesim.inputs sim in
    inputs.clocking.clear := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.clocking.clear := Bits.gnd;
    waves, sim
  ;;

  let rec drive_cycles h (_ : Step.O_data.t) =
    Int.incr cycle_counter;
    let o = Step.cycle h Step.input_hold in
    drive_cycles h o
  ;;

  module Input = struct
    (* Send a single set of dynamic inputs and record which cycle it was sent on. *)
    let rec send_dynamic_inputs h dynamic_inputs =
      let o = Step.cycle h { Step.input_hold with dynamic_inputs; start = vdd } in
      if Bits.to_bool o.before_edge.ready
         (* Because we are checking [before_edge], we have to subtract 1 to get the cycle
            we sent on. *)
      then (
        let cycle = !cycle_counter - 1 in
        cycle)
      else send_dynamic_inputs h dynamic_inputs
    ;;

    (* Send all the dynamic inputs one after another and record which cycle each set was
       sent. *)
    let send (h @ local) ~dynamic_inputs_list =
      List.map dynamic_inputs_list ~f:(send_dynamic_inputs h) [@nontail]
    ;;
  end

  let to_float v ({ signed; f_bits; i_bits = _ } : Precision.t) =
    if signed
    then Fixed_signed.create f_bits v |> Fixed_signed.to_float
    else Fixed_unsigned.create f_bits v |> Fixed_unsigned.to_float
  ;;

  let of_float v ({ signed; i_bits; f_bits } : Precision.t) =
    if signed
    then Fixed_signed.of_float i_bits f_bits v |> Fixed_signed.signal
    else Fixed_unsigned.of_float i_bits f_bits v |> Fixed_unsigned.signal
  ;;

  module Step_output = struct
    type t =
      { q : float option Output.t
      ; overflow : bool option
      ; cycle : int
      }
    [@@deriving compare, sexp_of]

    let preprocess output ~expect =
      { cycle = output.cycle
      ; q =
          (* Mask out any fields where the expected value is [None] (i.e. don't care). *)
          Output.map2 expect.q output.q ~f:(fun expect out ->
            match expect with
            | None -> None
            | Some _ -> out)
      ; overflow =
          (match expect.overflow with
           | None -> None
           | Some _ -> output.overflow)
      }
    ;;

    let of_o_data (o : Step.O_data.t) =
      { q =
          Output.map2 o.after_edge.q Output.precision ~f:(fun v p -> Some (to_float v p))
      ; overflow = Some (Bits.to_bool o.after_edge.overflow)
      ; cycle = !cycle_counter
      }
    ;;

    let collect_outputs h ~n o =
      let rec helper h ~n l (o : Step.O_data.t) =
        (* Check if we are done. *)
        if List.length l = n
        then List.rev l
        else (
          (* Check if we have a new output. *)
          let l = if Bits.to_bool o.after_edge.valid then of_o_data o :: l else l in
          let o = Step.cycle h Step.input_hold in
          helper h ~n l o)
      in
      helper h ~n [] o
    ;;
  end

  let validate_outputs
    ~verbose
    ~static_inputs
    ~dynamic_inputs_list
    ~input_send_times
    ~outputs
    =
    N_ary.List3.iter_exn
      dynamic_inputs_list
      input_send_times
      outputs
      ~f:(fun dynamic_inputs send_time step_output ->
        let expect =
          let ~q, ~overflow = model ~static_inputs ~dynamic_inputs in
          { Step_output.q; overflow; cycle = send_time + latency }
        in
        if verbose
        then print_s [%message "" (step_output : Step_output.t) (expect : Step_output.t)];
        [%test_result: Step_output.t]
          (Step_output.preprocess step_output ~expect)
          ~expect
          ~message:(Sexp.to_string [%message (dynamic_inputs : float Dynamic_inputs.t)]))
  ;;

  let testbench ~static_inputs ~dynamic_inputs_list h _ =
    (* Start cycle counter. *)
    Step.spawn' h (fun ev o -> drive_cycles ev o);
    (* Set the static inputs *)
    Step.delay
      h
      { Step.input_hold with
        static_inputs =
          Static_inputs.map2 static_inputs Static_inputs.precision ~f:of_float
      };
    (* Drive the input. *)
    let dynamic_inputs_list_converted =
      List.map dynamic_inputs_list ~f:(fun dynamic_inputs ->
        Dynamic_inputs.map2 dynamic_inputs Dynamic_inputs.precision ~f:of_float)
    in
    let spawn_send =
      Step.spawn h (fun ev _ ->
        Input.send ev ~dynamic_inputs_list:dynamic_inputs_list_converted)
    in
    (* Collect the outputs *)
    let spawn_recv =
      Step.spawn h (fun ev o ->
        Step_output.collect_outputs ev ~n:(List.length dynamic_inputs_list) o)
    in
    (* Wait for the results and validate them. *)
    let input_send_times = Step.wait_for h spawn_send in
    let outputs = Step.wait_for h spawn_recv in
    ~input_send_times, ~outputs
  ;;

  let run_test ?(verbose = false) ?(waves = false) ~static_inputs ~dynamic_inputs_list () =
    let waves, simulator = create_and_clear_sim ~waves () in
    let testbench = testbench ~static_inputs ~dynamic_inputs_list in
    let ~input_send_times, ~outputs = Step.run_until_finished () ~simulator ~testbench in
    Option.iter waves ~f:(Fn.flip Waveform.Serialize.marshall "debug.hardcamlwaveform");
    validate_outputs
      ~verbose
      ~static_inputs
      ~dynamic_inputs_list
      ~input_send_times
      ~outputs
  ;;

  module Quickcheck_generators = struct
    open Quickcheck.Generator.Let_syntax

    (* Build a quickcheck-generator for an entire Hardcaml [Interface_with_precision]. *)
    module Of_hardcaml (X : Hardcaml_computation_dsl.Interface_with_precision.S) = struct
      let generator =
        X.precision
        |> X.to_alist
        |> List.map ~f:(fun (tag, precision) ->
          let%map x = Q_format_utils.generate precision in
          tag, x)
        |> Quickcheck.Generator.all
        >>| X.of_alist
      ;;
    end

    module Dynamic_inputs_gen = Of_hardcaml (Dynamic_inputs)
    module Static_inputs_gen = Of_hardcaml (Static_inputs)

    let dynamic_inputs = Dynamic_inputs_gen.generator
    let static_inputs = Static_inputs_gen.generator
  end
end
