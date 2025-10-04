open! Core
open! Bonsai_synth_core
open! Bonsai.Let_syntax

module Event = struct
  type t =
    | Start
    | Stop
end

module ADSR = struct
  module State = struct
    type t =
      | Active of { start : Uptime.t }
      | Releasing of
          { stop_value : float
          ; stop : Uptime.t
          }
      | Inactive
  end

  let active_value t ~attack ~decay ~sustain =
    let open Float.O in
    match () with
    | _ when t >= attack +. decay -> sustain
    | _ when t >= attack ->
      let proportion = (t - attack) / decay in
      1. - (proportion * (1. - sustain))
    | _ when t >= 0. -> t / attack
    | _ -> 0.
  ;;

  let releasing_value t ~stop_value ~release =
    let open Float.O in
    if t <= 0.
    then stop_value
    else if t <= release
    then stop_value * (t / release)
    else 0.
  ;;

  let create
        ?(attack = Bonsai.return (Time_ns.Span.of_int_ms 10))
        ?(decay = Bonsai.return Time_ns.Span.zero)
        ?(sustain = Bonsai.return 1.)
        ?(release = Bonsai.return Time_ns.Span.zero)
        graph
    =
    let state, set_state = Bonsai.state State.Inactive graph in
    let attack = attack >>| Time_ns.Span.to_sec in
    let decay = decay >>| Time_ns.Span.to_sec in
    let release = release >>| Time_ns.Span.to_sec in
    let get_now = Uptime.get_current graph in
    let value =
      match%sub state with
      | Inactive -> return None
      | Active { start } ->
        let%arr now = Uptime.current graph
        and start = start
        and attack = attack
        and decay = decay
        and sustain = sustain
        and sample_length = Sample_rate.sample_length_sec graph in
        let t0 = Time_ns.Span.to_sec (Uptime.sub now start) in
        Some
          (Block.make (fun idx ->
             let t = t0 +. (Float.of_int idx *. sample_length) in
             active_value t ~attack ~decay ~sustain))
      | Releasing { stop_value; stop } ->
        (* Make the note inactive when release finished *)
        after_tick
          (let%arr set_state = set_state
           and get_now = get_now
           and release = release
           and stop = stop in
           let%bind.Effect now = get_now in
           let t = Uptime.sub now stop |> Time_ns.Span.to_sec in
           if Float.O.(t >= release) then set_state Inactive else Effect.return ())
          graph;
        let%arr now = Uptime.current graph
        and release = release
        and stop = stop
        and stop_value = stop_value
        and sample_length = Sample_rate.sample_length_sec graph in
        let t0 = Time_ns.Span.to_sec (Uptime.sub now stop) in
        Some
          (Block.make (fun idx ->
             let t = t0 +. (Float.of_int idx *. sample_length) in
             releasing_value t ~stop_value ~release))
    in
    let handle_event =
      match%sub state with
      | Inactive | Releasing _ ->
        let%arr get_now = get_now
        and set_state = set_state in
        fun event ->
          (match (event : Event.t) with
           | Stop -> Effect.return ()
           | Start ->
             let%bind.Effect now = get_now in
             set_state (Active { start = now }))
      | Active { start } ->
        let%arr start = start
        and get_now = get_now
        and set_state = set_state
        and attack = attack
        and decay = decay
        and sustain = sustain
        and release = release in
        fun event ->
          let%bind.Effect now = get_now in
          (match (event : Event.t) with
           | Start -> set_state (Active { start = now })
           | Stop ->
             (match Float.O.(release > 0.) with
              | false ->
                (* no release needed, swap immediately to inactive *)
                set_state Inactive
              | true ->
                let stop_t = Uptime.sub now start |> Time_ns.Span.to_sec in
                let stop_value = active_value stop_t ~attack ~decay ~sustain in
                set_state (Releasing { stop_value; stop = now })))
    in
    value, handle_event
  ;;
end

let adsr = ADSR.create
