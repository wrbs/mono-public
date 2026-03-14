open! Core
open! Hardcaml

module T = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; start : 'a
    ; stop : 'a
    ; abort : 'a
    }
  [@@deriving hardcaml]
end

module Pushback = struct
  module From_sender = struct
    type 'a t = { valid : 'a } [@@deriving hardcaml]
  end

  module From_receiver = struct
    type 'a t = { ready : 'a } [@@deriving hardcaml]
  end
end

module Always_helper = struct
  open Signal
  open Always

  type nonrec t' =
    { stream_out : Variable.t T.t
    ; valid : Variable.t
    ; started : Variable.t
    ; abort_triggered : Variable.t
    ; reset_triggered : Variable.t
    }

  let ensure_started t =
    unless t.started.value [ t.stream_out.start <-- vdd; t.started <-- vdd ]
  ;;

  let stop_if_started t =
    when_
      (t.stream_out.start.value |: t.started.value)
      [ t.stream_out.stop <-- vdd; t.started <-- gnd ]
  ;;

  let emit t data = proc [ t.valid <-- vdd; t.stream_out.data <-- data ]
  let abort t = t.abort_triggered <-- vdd
  let reset t = t.reset_triggered <-- vdd

  type t =
    { ensure_started : Always.t
    ; stop_if_started : Always.t
    ; emit : Signal.t -> Always.t
    ; abort : Always.t
    ; reset : Always.t
    ; abort_no_reset : Always.t
    }

  let of_t' t' =
    { ensure_started = ensure_started t'
    ; stop_if_started = stop_if_started t'
    ; emit = emit t'
    ; abort = proc [ abort t'; reset t' ]
    ; reset = reset t'
    ; abort_no_reset = abort t'
    }
  ;;

  let compile_with scope ~reg_spec ~upstream_abort ~reset f =
    let open Signal in
    let open Always in
    let%hw.T.Of_always stream_out = T.Of_always.wire zero in
    let%hw_var started = Variable.reg ~width:1 reg_spec in
    let%hw_var abort_triggered = Variable.wire ~default:gnd () in
    let%hw_var reset_triggered = Variable.wire ~default:gnd () in
    let%hw_var valid = Variable.wire ~default:gnd () in
    let t' = { stream_out; started; abort_triggered; reset_triggered; valid } in
    let impl = f (of_t' t') in
    compile
      [ if_ upstream_abort [ abort_triggered <-- vdd ] @@ else_ impl
      ; when_ reset_triggered.value ((started <-- gnd) :: reset)
      ; when_
          abort_triggered.value
          [ T.Of_always.assign stream_out (T.Of_signal.zero ())
          ; when_ started.value [ stream_out.abort <-- vdd ]
          ; started <-- gnd
          ]
      ];
    T.Of_always.value stream_out, ~valid:valid.value
  ;;
end

module Start_buffer_state = struct
  type t =
    | Waiting
    | Queued_start
    | Emitting
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let start_buffer scope (in_ : _ T.t) ~ready_to_start ~ready_for_data ~reg_spec =
  let open Signal in
  let scope = Scope.sub_scope scope "start_buffer" in
  let%hw.Always.State_machine state =
    Always.State_machine.create (module Start_buffer_state) reg_spec
  in
  let%hw_var start = Always.Variable.wire ~default:gnd () in
  Always.(
    compile
      [ state.switch
          [ ( Waiting
            , [ when_
                  in_.start
                  [ if_ ready_to_start [ start <-- vdd; state.set_next Emitting ]
                    @@ else_ [ state.set_next Queued_start ]
                  ]
              ] )
          ; ( Queued_start
            , [ if_ in_.abort [ state.set_next Waiting ]
                @@ else_
                     [ when_ ready_to_start [ start <-- vdd; state.set_next Emitting ] ]
              ] )
          ; ( Emitting
            , [ when_
                  (in_.abort |: (ready_for_data &: in_.stop))
                  [ state.set_next Waiting ]
              ] )
          ]
      ]);
  let emitting = state.is Emitting in
  let%hw.T.Of_signal out =
    { data = in_.data
    ; start = start.value
    ; stop = mux2 emitting in_.stop gnd
    ; abort = mux2 emitting in_.abort gnd
    }
  in
  out, ~emitting
;;

module Gate_state = struct
  type t =
    | Waiting
    | Running
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let gate scope (in_ : _ T.t) ~start_valid ~reg_spec =
  let open Signal in
  let scope = Scope.sub_scope scope "gate" in
  let%hw.Always.State_machine state =
    Always.State_machine.create (module Gate_state) reg_spec
  in
  let%hw_var start = Always.Variable.wire ~default:gnd () in
  Always.(
    compile
      [ state.switch
          [ Waiting, [ when_ (in_.start &: start_valid) [ start <-- vdd ] ]
          ; Running, [ when_ (in_.abort |: in_.stop) [ state.set_next Waiting ] ]
          ]
      ]);
  let%hw running = state.is Running in
  let o : _ T.t =
    { data = in_.data
    ; start = start.value
    ; stop = mux2 running in_.stop gnd
    ; abort = mux2 running in_.abort gnd
    }
  in
  o, ~running
;;

include T
