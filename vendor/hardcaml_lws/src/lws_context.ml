open Core
open Hardcaml

module Simulator_specific = struct
  module Cyclesim = struct
    type ('i, 'o) t = { mutable simulator : ('i, 'o) Cyclesim.t }

    let simulator t = t.simulator
    let create ~simulator = { simulator }
  end

  module Evsim = struct
    module Evsim = Hardcaml_event_driven_sim.Two_state_simulator

    type t = { simulator : Evsim.Simulator.t }

    let simulator t = t.simulator
    let create ~simulator = { simulator }
  end

  type ('i, 'o) t =
    | Cyclesim of ('i, 'o) Cyclesim.t
    | Evsim of Evsim.t
    | Cosim
end

type ('i, 'o) internal =
  { simulator_specific : ('i, 'o) Simulator_specific.t
  ; mutable waveform : Hardcaml_waveterm.Waveform.t option
  ; trigger_by_clock_port_name : Lws_trigger.t String.Map.t
  }

type ('i, 'o) t =
  { inputs : 'i
  ; outputs : 'o Before_and_after_edge.t
  ; internal : ('i, 'o) internal
  }

let waveform t = t.internal.waveform
let simulator_specific t = t.internal.simulator_specific

let resolve_trigger_by_clock_port_name t p =
  Map.find t.internal.trigger_by_clock_port_name p
;;

let spawn_in_clock_domain
  ~(here : [%call_pos])
  (handle : Lws.Handler.t @ local)
  (sim_ports : _ t)
  ~clock_domain
  thunk
  =
  let trigger =
    match clock_domain with
    | `Port_name port_name ->
      (match resolve_trigger_by_clock_port_name sim_ports port_name with
       | None ->
         raise_s
           [%message "No clock port matching the given port name" ~_:(port_name : string)]
       | Some x -> x)
  in
  Lws.spawn ~trigger ~here handle thunk
;;

module M (I : T1) (O : T1) = struct
  type nonrec t = (Bits.t ref I.t, Bits.t ref O.t) t
end

module Private = struct
  let create ~inputs ~outputs ~waveform ~simulator_specific ~trigger_by_clock_port_name =
    { inputs
    ; outputs
    ; internal = { simulator_specific; waveform; trigger_by_clock_port_name }
    }
  ;;

  module Simulator_specific = struct
    module Cyclesim = struct
      let create = Simulator_specific.Cyclesim.create

      let set_simulator_and_waveform t simulator waveform =
        match t.internal.simulator_specific with
        | Cyclesim cyclesim ->
          cyclesim.simulator <- simulator;
          t.internal.waveform <- waveform
        | Evsim _ -> failwith "Trying to set Cyclesim data on an Evsim context"
        | Cosim -> failwith "Trying to set Cyclesim data on an Cosim context"
      ;;
    end

    module Evsim = struct
      let create = Simulator_specific.Evsim.create
    end
  end
end

module Waveform = struct
  type 'a with_options = 'a Hardcaml_waveterm_kernel.Waveform.with_options

  let print'
    f
    ()
    ?display_rules
    ?display_width
    ?display_height
    ?display_values
    ?wave_width
    ?signals_width
    ?start_cycle
    ?signals_alignment
    t
    =
    Option.iter t.internal.waveform ~f:(fun waves ->
      f
        ?display_rules
        ?display_width
        ?display_height
        ?display_values
        ?wave_width
        ?signals_width
        ?start_cycle
        ?signals_alignment
        waves)
  ;;

  let print ?channel = print' (Hardcaml_waveterm.Waveform.print ?channel) ()

  let expect ?show_digest ?serialize_to =
    print' (Hardcaml_waveterm.Waveform.expect ?show_digest ?serialize_to) ()
  ;;

  let expect_exact ?show_digest ?serialize_to =
    print' (Hardcaml_waveterm.Waveform.expect_exact ?show_digest ?serialize_to) ()
  ;;
end
