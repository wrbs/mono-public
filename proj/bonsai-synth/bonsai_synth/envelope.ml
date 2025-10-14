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
        and start
        and attack
        and decay
        and sustain
        and sample_length = Sample_rate.sample_length_sec graph in
        let t0 = Time_ns.Span.to_sec (Uptime.sub now start) in
        Some
          (Block.make (fun idx ->
             let t = t0 +. (Float.of_int idx *. sample_length) in
             active_value t ~attack ~decay ~sustain))
      | Releasing { stop_value; stop } ->
        let%arr now = Uptime.current graph
        and release
        and stop
        and stop_value
        and sample_length = Sample_rate.sample_length_sec graph in
        let t0 = Time_ns.Span.to_sec (Uptime.sub now stop) in
        Some
          (Block.make (fun idx ->
             let t = t0 +. (Float.of_int idx *. sample_length) in
             releasing_value t ~stop_value ~release))
    in
    let reset_if_silent =
      let%arr value and set_state in
      let%bind.Option block = value in
      let last_sample = Block.last block in
      if Float.O.(abs last_sample <= 0.00001) then Some (set_state Inactive) else None
    in
    after_tick' reset_if_silent graph;
    let handle_event =
      match%sub state with
      | Inactive | Releasing _ ->
        let%arr get_now and set_state in
        fun event ->
          (match (event : Event.t) with
           | Stop -> Effect.return ()
           | Start ->
             let%bind.Effect now = get_now in
             set_state (Active { start = now }))
      | Active { start } ->
        let%arr start
        and get_now
        and set_state
        and attack
        and decay
        and sustain
        and release in
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
