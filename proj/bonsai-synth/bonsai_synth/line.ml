open! Core
open! Bonsai_synth_core
open! Bonsai.Let_syntax

type t =
  { value : Block.t Bonsai.t
  ; set : (float -> unit Effect.t) Bonsai.t
  ; queue : ((Time_ns.Span.t * float) list -> unit Effect.t) Bonsai.t
  }

type time = { sec : float } [@@unboxed]

module Ramp = struct
  type t =
    { value : float
    ; last_updated : time
    ; target_value : float
    ; target_time : time
    ; remaining : (float * time) list
    }

  let update' t ~now =
    match now.sec >=. t.target_time.sec with
    | true -> `Finished { t with value = t.target_value; last_updated = t.target_time }
    | false ->
      let amount_remaining = t.target_value -. t.value in
      let time_remaining = t.target_time.sec -. t.last_updated.sec in
      let since_last = now.sec -. t.last_updated.sec in
      let value = t.value +. (since_last /. time_remaining *. amount_remaining) in
      `Stepped { t with value; last_updated = now }
  ;;

  let rec update t ~now =
    match update' t ~now with
    | `Stepped t -> `Continue t
    | `Finished t ->
      (match t.remaining with
       | [] -> `Stop t.value
       | (target_value, target_time) :: rest ->
         update { t with target_value; target_time; remaining = rest } ~now)
  ;;
end

module State = struct
  type t =
    | Fixed of float
    | Ramping of Ramp.t

  let value t =
    match t with
    | Fixed value | Ramping { value; _ } -> value
  ;;

  let fixed v = Fixed v

  let update t ~now =
    match t with
    | Fixed v -> Fixed v
    | Ramping ramp ->
      (match Ramp.update ramp ~now with
       | `Continue ramp' -> Ramping ramp'
       | `Stop final_value -> Fixed final_value)
  ;;

  let start_ramp t ~uptime ~queue =
    let start = Uptime.secs_since_start uptime in
    match
      List.map queue ~f:(fun (span, target_value) ->
        target_value, { sec = start +. Time_ns.Span.to_sec span })
    with
    | [] -> t
    | (target_value, target_time) :: remaining ->
      Ramping
        { value = value t
        ; last_updated = { sec = start }
        ; target_value
        ; target_time
        ; remaining
        }
      |> (* handle any immediate transitions *)
      update ~now:{ sec = start }
  ;;
end

let create ?(init = 0.) graph =
  let state, set_state = Bonsai.state (State.fixed init) graph in
  let set =
    let%arr set_state in
    fun value -> set_state (State.fixed value)
  in
  let queue =
    let%arr state
    and set_state
    and get_uptime = Uptime.get_current graph in
    fun queue ->
      let%bind.Effect uptime = get_uptime in
      set_state (State.start_ramp state ~uptime ~queue)
  in
  let value =
    match%sub state with
    | Fixed v -> v >>| Block.const
    | Ramping ramp ->
      let%sub block, set_next_state =
        let%arr sample_secs = Sample_rate.sample_length_sec graph
        and uptime = Uptime.current graph
        and ramp
        and set_state in
        let block_start = Uptime.secs_since_start uptime in
        let (next_state, _), block =
          Block.unfold (State.Ramping ramp, block_start) ~f:(fun (state, t) ->
            let value = State.value state in
            let t = t +. sample_secs in
            let state' = State.update state ~now:{ sec = t } in
            (state', t), value)
        in
        block, set_state next_state
      in
      after_tick set_next_state graph;
      block
  in
  { value; set; queue }
;;
