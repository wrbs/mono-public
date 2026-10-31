open Core
open Hardcaml

type 'context t =
  { base_unwrapped_simulator : ('i, 'o) Cyclesim.t
  ; evaluator : Lws_core.Evaluator.t
  ; default_trigger : Lws_core.Trigger.t Or_error.t
  ; triggers_and_periods : (Lws_core.Trigger.t * int) iarray
  ; context : 'context
  ; shared_config : Lws.Config.t
  ; assertions : Assertions.t
  }
  constraint 'context = ('i, 'o) Lws_context.t
[@@deriving fields ~getters]

let resolve_trigger_by_clock_name (t : _ t) name =
  Lws_context.resolve_trigger_by_clock_port_name t.context name
;;

let schedule_task ?trigger ~(here : [%call_pos]) t thunk =
  let trigger =
    match trigger with
    | Some x -> x
    | None ->
      (match t.default_trigger with
       | Ok x -> x
       | Error error ->
         raise_s
           [%message
             "Failed to get default trigger. You probably need to provide the ~trigger \
              argument to the [schedule_task] function call."
               (error : Error.t)])
  in
  Lws_core.Evaluator.add_task t.evaluator ~created_at:here ~trigger (fun h ->
    thunk h t.context)
;;

let cycle_num (t : _ t) = Lws_core.Evaluator.step_number t.evaluator

let active_simulator (t : _ t) =
  match Lws_context.simulator_specific t.context with
  | Cyclesim x -> Lws_context.Simulator_specific.Cyclesim.simulator x
  | Evsim _ -> assert false
  | Cosim -> assert false
;;

let cycle_impl (t : _ t) sim =
  let step_number = Lws_core.Evaluator.step_number t.evaluator in
  let triggers @ local =
    Iarray.Local.filter_map t.triggers_and_periods ~f:(fun (trigger, period) -> exclave_
      match step_number mod period = 0 with
      | true -> Some trigger
      | false -> None)
  in
  Lws_core.Evaluator.step t.evaluator triggers;
  Cyclesim.cycle sim
;;

let cycle ?(n = 1) (t : _ t) =
  let sim = active_simulator t in
  for _ = 1 to n do
    cycle_impl t sim
  done
;;

let poll_task (t : _ t) ev =
  let sim = active_simulator t in
  let start_step = Lws_core.Evaluator.step_number t.evaluator in
  while Or_null.is_null (Lws_core.Task.read ev) do
    Option.iter t.shared_config.timeout ~f:(fun timeout ->
      if Lws_core.Evaluator.step_number t.evaluator - start_step >= timeout
      then raise_s [%message "LWS simulation timed out" (timeout : int)]);
    cycle_impl t sim
  done;
  Or_null.value_exn (Lws_core.Task.read ev)
;;

let run ?trigger ~(here : [%call_pos]) t thunk =
  poll_task t (schedule_task ?trigger ~here t thunk)
;;

let resolve_trigger_by_period (t : _ t) period =
  Iarray.find_map t.triggers_and_periods ~f:(fun (trigger, trigger_period) ->
    if period = trigger_period then Some trigger else None)
;;

let waveform_trace (t : _ Lws_context.t t) (mode : [ `on | `off ]) =
  match Lws_context.simulator_specific t.context with
  | Cyclesim _cyclesim ->
    (match mode with
     | `off ->
       Lws_context.Private.Simulator_specific.Cyclesim.set_simulator_and_waveform
         t.context
         t.base_unwrapped_simulator
         None
     | `on ->
       let waveform, sim = Cyclesim.Waveform.create t.base_unwrapped_simulator in
       Lws_context.Private.Simulator_specific.Cyclesim.set_simulator_and_waveform
         t.context
         sim
         (Some waveform))
  | Evsim _ | Cosim -> raise_s [%message "waveform_trace is only supported for cyclesim"]
;;

module Backend_specific_config = struct
  module Simulation_backend = Hardcaml_verilator.Simulation_backend

  type t =
    { cyclesim_config : Cyclesim.Config.t
    ; simulation_backend : Simulation_backend.t
    ; waves : bool
    }

  let default =
    { cyclesim_config = Cyclesim.Config.default
    ; simulation_backend = Hardcaml
    ; waves = false
    }
  ;;

  let default_with_clock_mode clock_mode =
    { cyclesim_config = { Cyclesim.Config.default with clock_mode }
    ; simulation_backend = Hardcaml
    ; waves = false
    }
  ;;
end

module With_interface (I : Interface.S) (O : Interface.S) = struct
  module Sim = Cyclesim.With_interface (I) (O)
  module Vsim = Hardcaml_verilator.With_interface (I) (O)

  type sim_context = (Bits.t ref I.t, Bits.t ref O.t) Lws_context.t
  type create_fn = Scope.t -> Signal.t I.t -> Signal.t O.t

  let create
    ?(config : Lws.Config.t = Lws.Config.default)
    ?(backend_specific_config = Backend_specific_config.default)
    (create_fn : create_fn)
    =
    let shared_config = config in
    let config = backend_specific_config in
    let base_unwrapped_simulator, assertions =
      let scope = Runtime_utils.create_scope_from_shared_config shared_config in
      let create_fn = create_fn scope in
      let circuit_config =
        if shared_config.assertions
        then Some (Assertions.circuit_config_with_assertions scope)
        else None
      in
      let sim =
        match config.simulation_backend with
        | Hardcaml -> Sim.create ~config:config.cyclesim_config ?circuit_config create_fn
        | Verilator cache ->
          (match circuit_config with
           | None -> ()
           | Some (_ : Circuit.Config.t) ->
             raise_s
               [%message
                 "Assertions are not supported with the verilator simulation backend"]);
          Vsim.create ~cache ~config:config.cyclesim_config create_fn
      in
      let assertions, sim =
        if shared_config.assertions
        then Assertions.trace sim (Scope.assertion_manager scope)
        else Assertions.trace sim None
      in
      sim, assertions
    in
    let waveform, sim =
      Cyclesim.Waveform.create_if ~enabled:config.waves base_unwrapped_simulator
    in
    let default_trigger, trigger_by_clock_port_name, triggers_and_periods =
      match config.cyclesim_config.clock_mode with
      | All_one_domain ->
        let trigger = Lws_core.Trigger.create "clock" in
        Ok trigger, String.Map.singleton "clock" trigger, [: trigger, 1 :]
      | By_input_clocks input_clocks ->
        let names_triggers_and_periods =
          List.map input_clocks ~f:(fun c ->
            let name = Cyclesim_clock_domain.Name.to_string c.name in
            let trigger = Lws_core.Trigger.create name in
            name, trigger, c.period)
        in
        let m =
          List.map names_triggers_and_periods ~f:(fun (n, trigger, _) -> n, trigger)
          |> String.Map.of_alist_exn
        in
        let user_provided_triggers_and_periods =
          names_triggers_and_periods
          |> List.map ~f:(fun (_, trigger, period) -> trigger, period)
        in
        let default_trigger = Lws_core.Trigger.create "default" in
        let triggers_and_periods =
          Iarray.of_list ((default_trigger, 1) :: user_provided_triggers_and_periods)
        in
        Ok default_trigger, m, triggers_and_periods
    in
    let evaluator = Lws_core.Evaluator.create () in
    let sim_context =
      let inputs = Cyclesim.inputs sim in
      let outputs =
        let before_edge = Cyclesim.outputs ~clock_edge:Before sim in
        let after_edge = Cyclesim.outputs ~clock_edge:After sim in
        { Before_and_after_edge.before_edge; after_edge }
      in
      let simulator_specific =
        Lws_context.Simulator_specific.Cyclesim
          (Lws_context.Private.Simulator_specific.Cyclesim.create ~simulator:sim)
      in
      Lws_context.Private.create
        ~inputs
        ~outputs
        ~waveform
        ~simulator_specific
        ~trigger_by_clock_port_name
    in
    { base_unwrapped_simulator
    ; evaluator
    ; default_trigger
    ; triggers_and_periods
    ; context = sim_context
    ; shared_config
    ; assertions
    }
  ;;
end

include functor Lws_runtime.To_polymorphic
