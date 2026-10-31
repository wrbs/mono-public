open Core
open Hardcaml
module Evsim = Hardcaml_event_driven_sim.Two_state_simulator

module Backend_specific_config = struct
  module Clock = struct
    type t =
      { port_name : string
      ; time : int
      ; initial_delay : int
      }
  end

  module Common = struct
    type t =
      { clocks : Clock.t list
      ; default_clock_name : string option
      (** When specified, all tasks by default will be spawned in the above clock domain,
          if ~trigger is not specified. *)
      ; waves : bool
      }
  end

  type t =
    { common : Common.t
    ; backend : Evsim.Config.t
    }
end

type 'context t =
  { step : unit -> unit
  ; evaluator : Lws_core.Evaluator.t
  ; clock_port_names_and_triggers : (string * Lws_core.Trigger.t) iarray
  ; context : 'context
  ; default_trigger : Lws_trigger.t option
  ; shared_config : Lws.Config.t
  }
  constraint 'context = ('i, 'o) Lws_context.t
[@@deriving fields ~getters]

let get_default_trigger t = t.default_trigger

let resolve_trigger_by_clock_name (t : _ t) name =
  Lws_context.resolve_trigger_by_clock_port_name t.context name
;;

let poll_task t task =
  let start_step = Lws_core.Evaluator.step_number t.evaluator in
  while Or_null.is_null (Lws_core.Task.read task) do
    Option.iter t.shared_config.timeout ~f:(fun timeout ->
      if Lws_core.Evaluator.step_number t.evaluator - start_step >= timeout
      then raise_s [%message "LWS simulation timed out" (timeout : int)]);
    t.step ()
  done;
  Or_null.value_exn (Lws_core.Task.read task)
;;

let schedule_task ?trigger ~(here : [%call_pos]) t thunk =
  let trigger =
    match trigger with
    | Some x -> x
    | None ->
      (match get_default_trigger t with
       | None ->
         raise_s
           [%message
             "Called schedule_task without a trigger argument, when config is missing a \
              default clock name!"]
       | Some x -> x)
  in
  Lws_core.Evaluator.add_task t.evaluator ~created_at:here ~trigger (fun h ->
    thunk h t.context)
;;

let run ?trigger ~(here : [%call_pos]) t thunk =
  poll_task t (schedule_task ?trigger ~here t thunk)
;;

module type S_sim = sig
  type t

  val delta_step : t -> unit
  val has_delta_updates : t -> bool
  val step : t -> unit

  module Expert : sig
    val schedule_external_set : t -> 'value Evsim.Simulator.Signal.t -> 'value -> unit
  end
end

module With_interface (I : Hardcaml.Interface.S) (O : Interface.S) = struct
  type sim_context = (Bits.t ref I.t, Bits.t ref O.t) Lws_context.t
  type create_fn = Scope.t -> Signal.t I.t -> Signal.t O.t

  type circuit_and_io =
    { circuit_processes : Evsim.Simulator.Process.t list
    ; input_ports : Evsim.Logic.t Hardcaml_event_driven_sim.Port.t I.t
    ; output_ports : Evsim.Logic.t Hardcaml_event_driven_sim.Port.t O.t
    ; internal : Evsim.Logic.t Hardcaml_event_driven_sim.Port.t list
    }

  let find_in_input (x : _ I.t) name =
    List.Assoc.find_exn (I.to_list (I.zip I.port_names x)) name ~equal:String.equal
  ;;

  let create'
    (type sim)
    ?(config : Lws.Config.t = Lws.Config.default)
    ~(backend_specific_config : Backend_specific_config.Common.t)
    ~create_circuit_and_io
    ~create_sim
    ~create_sim_specific
    (module Sim : S_sim with type t = sim)
    =
    let shared_config = config in
    let config = backend_specific_config in
    let clock_port_names_and_triggers =
      config.clocks
      |> List.map ~f:(fun c -> c.port_name, Lws_core.Trigger.create c.port_name)
      |> Iarray.of_list
    in
    let trigger_by_clock_port_name =
      clock_port_names_and_triggers |> Iarray.to_list |> String.Map.of_alist_exn
    in
    let evaluator = Lws_core.Evaluator.create () in
    let sim_context_inputs = I.map I.port_widths ~f:(fun w -> ref (Bits.zero w)) in
    let sim_context_outputs =
      Before_and_after_edge.create
        ~before_edge:(O.map O.port_widths ~f:(fun w -> ref (Bits.zero w)))
        ~after_edge:(O.map O.port_widths ~f:(fun w -> ref (Bits.zero w)))
    in
    let scope = Runtime_utils.create_scope_from_shared_config shared_config in
    let { circuit_processes; input_ports; output_ports; internal }, sim_data =
      create_circuit_and_io scope
    in
    let%tydi waveterm_processes, waveform =
      let open Hardcaml_event_driven_sim.Two_state_simulator.Waveterm in
      if config.waves
      then (
        let%tydi { processes; waveform } =
          create
            ~inputs:(I.to_list input_ports)
            ~outputs:(O.to_list output_ports)
            ~internal
        in
        processes, Some waveform)
      else [], None
    in
    let clock_processes =
      config.clocks
      |> List.map ~f:(fun { port_name; time; initial_delay } ->
        let input_port = find_in_input input_ports port_name in
        find_in_input sim_context_inputs port_name := Bits.empty;
        Evsim.Simulator.create_clock
          ~initial_delay
          input_port.signal
          ~time
          ~toggle:Evsim.Logic.( ~: ))
    in
    let clock_triggers = Iarray.map ~f:snd clock_port_names_and_triggers in
    let clock_ports =
      List.map config.clocks ~f:(fun c -> find_in_input input_ports c.port_name)
      |> Iarray.of_list
    in
    let processes =
      List.concat [ circuit_processes; clock_processes; waveterm_processes ]
    in
    let simulator = create_sim processes scope sim_data in
    let step () =
      (* Simulator.step progresses to the next time step with scheduled updates, but
         doesn't run any of the delta updates. *)
      let clock_values_before =
        Iarray.map clock_ports ~f:(fun p -> Evsim.Simulator.Signal.read p.signal)
      in
      let outputs_before =
        O.map output_ports ~f:(fun p -> Evsim.Simulator.Signal.read p.signal)
      in
      while Sim.has_delta_updates simulator do
        Sim.delta_step simulator
      done;
      let clock_values_after =
        Iarray.map clock_ports ~f:(fun p -> Evsim.Simulator.Signal.read p.signal)
      in
      let outputs_after =
        O.map output_ports ~f:(fun p -> Evsim.Simulator.Signal.read p.signal)
      in
      let triggers =
        Iarray.map2_exn
          clock_triggers
          (Iarray.zip_exn clock_values_before clock_values_after)
          ~f:(fun trigger (b, a) ->
            let is_rising = (not (Bits.to_bool b)) && Bits.to_bool a in
            if is_rising then Some trigger else None)
        |> Iarray.filter_map ~f:Fn.id
      in
      if not (Iarray.is_empty triggers)
      then (
        O.iter2 ~f:( := ) sim_context_outputs.before_edge outputs_before;
        O.iter2 ~f:( := ) sim_context_outputs.after_edge outputs_after;
        Lws_core.Evaluator.step evaluator triggers;
        I.iter2 sim_context_inputs input_ports ~f:(fun r p ->
          let should_update =
            (not (Bits.is_empty !r))
            && not (Bits.equal !r (Evsim.Simulator.Signal.read p.signal))
          in
          if should_update then Sim.Expert.schedule_external_set simulator p.signal !r));
      Sim.step simulator
    in
    let sim_context =
      Lws_context.Private.create
        ~inputs:sim_context_inputs
        ~outputs:sim_context_outputs
        ~waveform
        ~simulator_specific:(create_sim_specific simulator)
        ~trigger_by_clock_port_name
    in
    let default_trigger =
      Option.map config.default_clock_name ~f:(fun name ->
        match Map.find trigger_by_clock_port_name name with
        | None ->
          raise_s
            [%message "Specified default_clock_name does not exists!" (name : string)]
        | Some x -> x)
    in
    { step
    ; evaluator
    ; clock_port_names_and_triggers
    ; context = sim_context
    ; default_trigger
    ; shared_config
    }
  ;;

  let create
    ?config
    ~(backend_specific_config : Backend_specific_config.t)
    (create_fn : Scope.t -> Signal.t I.t -> Signal.t O.t)
    =
    let module Sim = Evsim.With_interface (I) (O) in
    create'
      ?config
      ~backend_specific_config:backend_specific_config.common
      ~create_circuit_and_io:(fun scope ->
        let%tydi { processes = circuit_processes
                 ; input = input_ports
                 ; output = output_ports
                 ; internal
                 ; memories = _
                 }
          =
          Sim.create ~config:backend_specific_config.backend (create_fn scope)
        in
        { circuit_processes; input_ports; output_ports; internal }, ())
      ~create_sim:(fun processes _scope () -> Evsim.Simulator.create processes)
      ~create_sim_specific:(fun simulator ->
        Evsim (Lws_context.Private.Simulator_specific.Evsim.create ~simulator))
      (module Evsim.Simulator)
  ;;
end

include functor Lws_runtime.To_polymorphic
