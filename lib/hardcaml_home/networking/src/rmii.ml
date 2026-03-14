open! Core
open! Hardcaml_home
open! Signal
open! Signal_extended

let preamble_bits = of_bit_string "01"
let sfd_bits = of_bit_string "11"

module Hw = struct
  module Rx = struct
    type 'a t =
      { crsdv : 'a
      ; rxerr : 'a
      ; rxd : 'a [@bits 2] [@wave_format Binary]
      }
    [@@deriving hardcaml]
  end

  module Tx = struct
    type 'a t =
      { txen : 'a
      ; txd : 'a [@bits 2] [@wave_format Binary]
      }
    [@@deriving hardcaml]
  end
end

module Rx = struct
  module Sync_state = struct
    type t =
      | Wait
      | Wait_for_first_preamble
      | Preamble_1
      | Preamble_2
      | Wait_sfd
      | Frame
      | Drop
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let wait_for_sfd scope (hw : _ Hw.Rx.t) ~reg_spec =
    let%hw.Always.State_machine sync_state =
      Always.State_machine.create (module Sync_state) reg_spec
    in
    let%hw in_frame = sync_state.is Frame in
    Always.(
      let drop = sync_state.set_next Drop in
      let expect_preamble next_state =
        if_ (hw.rxd ==: preamble_bits) [ sync_state.set_next next_state ] [ drop ]
      in
      compile
        [ sync_state.switch
            [ Wait, [ when_ hw.crsdv [ sync_state.set_next Wait_for_first_preamble ] ]
            ; ( Wait_for_first_preamble
              , [ when_ (hw.rxd ==: preamble_bits) [ sync_state.set_next Preamble_1 ] ] )
            ; Preamble_1, [ expect_preamble Preamble_2 ]
            ; Preamble_2, [ expect_preamble Wait_sfd ]
            ; ( Wait_sfd
              , [ if_ (hw.rxd ==: sfd_bits) [ sync_state.set_next Frame ]
                  @@ else_ [ expect_preamble Wait_sfd ]
                ] )
            ; Frame, [] (* pass through to downstream *)
            ; Drop, [] (* wait for drop condition below*)
            ]
        ; when_ (~:in_frame &: hw.rxerr) [ drop ]
        ; when_ ~:(hw.crsdv) [ sync_state.set_next Wait ]
        ]);
    Hw.Rx.Of_signal.mux2 in_frame hw (Hw.Rx.Of_signal.zero ()), ~drop:(sync_state.is Drop)
  ;;

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; hw : 'a Hw.Rx.t
      ; ignore_crc : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { rx : 'a Packet_stream.t
      ; valid : 'a
      ; drop : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | B1
      | B2
      | B3
      | B0
      | Dropped
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    (* prev code didn't know about preamble/sfd because claude lied, so just
    patching in here *)
    let hw_sync, ~drop = wait_for_sfd scope i.hw ~reg_spec in
    let%hw.Hw.Rx.Of_signal hw_sync in
    let i = { i with hw = hw_sync } in
    (* prev code: *)
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw data =
      reg_fb reg_spec ~width:8 ~f:(fun cur -> shift_into cur ~add:i.hw.rxd ~at:`msb)
    in
    let%hw.Packet_stream.Of_always rx = Packet_stream.Of_always.wire zero in
    let%hw_var valid = Always.Variable.wire ~default:gnd () in
    let%hw_var sent_first_byte = Always.Variable.reg ~width:1 reg_spec in
    let%hw crc_good =
      let%hw crc =
        reg_fb reg_spec ~width:32 ~f:(fun crc ->
          mux2 i.hw.crsdv (Crc.update ~crc ~polynomial:Crc.crc32 i.hw.rxd) (ones 32))
      in
      i.ignore_crc |: (crc ==: of_bits (Crc.expected_residual ~polynomial:Crc.crc32))
    in
    let hw_err = i.hw.rxerr &: i.hw.crsdv in
    let%hw_var any_abort = Always.Variable.wire ~default:hw_err () in
    let%hw_var good_stop = Always.Variable.reg ~width:1 reg_spec in
    Always.(
      let maybe_frame_error = when_ ~:(i.hw.crsdv) [ any_abort <-- vdd ] in
      compile
        [ state.switch
            [ ( Idle
              , [ when_ good_stop.value [ good_stop <-- gnd; rx.stop <-- vdd ]
                ; when_ i.hw.crsdv [ state.set_next B1; rx.start <-- vdd ]
                ] )
            ; B1, [ state.set_next B2; maybe_frame_error ]
            ; B2, [ state.set_next B3; maybe_frame_error ]
            ; B3, [ state.set_next B0; maybe_frame_error ]
            ; ( B0
              , [ unless
                    hw_err
                    [ if_ (~:(i.hw.crsdv) &: ~:crc_good) [ any_abort <-- vdd ]
                      @@ else_
                           [ rx.data <-- data
                           ; valid <-- vdd
                           ; sent_first_byte <-- vdd
                           ; state.set_next B1
                           ; when_ ~:(i.hw.crsdv) [ good_stop <-- vdd ]
                           ]
                    ]
                ] )
            ; Dropped, []
            ]
        ; when_
            any_abort.value
            [ state.set_next Dropped
            ; when_ (sent_first_byte.value &: ~:(state.is Dropped)) [ rx.abort <-- vdd ]
            ]
        ; when_ ~:(i.hw.crsdv) [ sent_first_byte <-- gnd; state.set_next Idle ]
        ]);
    { rx = Packet_stream.Of_always.value rx; valid = valid.value; drop }
  ;;

  let name = "rmii_rx"

  include functor Component.Make
end

module Tx_ll = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; data : 'a [@bits 8]
      ; start : 'a (* pulse high to start packet*)
      ; stop_abort : 'a
      ; stop_with_crc : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { hw : 'a Hw.Tx.t
      ; ready_to_start : 'a
      ; ready_for_data : 'a
      ; sfd_sent : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Ready
      | Preamble
      | Data
      | CRC
      | IPG
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let ipg_bits = 96
  let ipg_cycles = ipg_bits / 2

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let count_bits = num_bits_to_represent (ipg_cycles - 1) in
    let%hw_var count = Always.Variable.reg ~width:count_bits reg_spec in
    let%hw_var buffer = Always.Variable.reg ~width:6 reg_spec in
    let%hw.Hw.Tx.Of_always hw = Hw.Tx.Of_always.wire zero in
    let%hw_var crc = Always.Variable.reg ~width:32 reg_spec in
    let%hw_var crc_to_emit = Always.Variable.wire ~default:(zero 32) () in
    (* Signals *)
    Always.(
      let start_ipg =
        proc [ state.set_next IPG; if_ hw.txen.value [ count <--. 0 ] [ count <--. 1 ] ]
      in
      let shift_out_crc ?(in_ = crc.value) () =
        proc [ crc <-- srl in_ ~by:2; hw.txd <-- sel_bottom in_ ~width:2 ]
      in
      compile
        [ state.switch
            [ ( Ready
              , [ when_
                    i.start
                    [ state.set_next Preamble
                    ; hw.txen <-- vdd
                    ; hw.txd <-- preamble_bits
                    ; incr count
                    ; count <--. 0
                    ; crc <-- ones 32
                    ]
                ] )
            ; ( Preamble
              , let preamble_cycles = (7 * 4) + 3 in
                let preamble_count = preamble_cycles - 1 (* first cycle = last one *) in
                [ if_ i.stop_abort [ start_ipg ]
                  @@ else_
                       [ hw.txen <-- vdd
                       ; if_
                           (count.value <:. preamble_count)
                           [ hw.txd <-- preamble_bits; incr count ]
                           [ hw.txd <-- sfd_bits; count <--. 0; state.set_next Data ]
                       ]
                ] )
            ; ( Data
              , let n = of_unsigned_int ~width:count_bits in
                let emit_unless_aborted rest =
                  [ if_ i.stop_abort [ start_ipg ] @@ else_ ((hw.txen <-- vdd) :: rest) ]
                in
                [ crc <-- Crc.update ~crc:crc.value ~polynomial:Crc.crc32 hw.txd.value
                ; switch
                    count.value
                    [ ( n 0
                      , [ if_
                            i.stop_with_crc
                            [ state.set_next CRC
                            ; hw.txen <-- vdd
                            ; if_
                                i.stop_abort
                                [ crc_to_emit
                                  <-- of_string "28'hBADCBAD"
                                      @: sel_bottom crc.value ~width:4
                                ]
                              @@ else_ [ crc_to_emit <-- ~:(crc.value) ]
                            ; shift_out_crc ~in_:crc_to_emit.value ()
                            ]
                          @@ else_
                               (emit_unless_aborted
                                  [ hw.txd <-- sel_bottom ~width:2 i.data
                                  ; buffer <-- drop_bottom ~width:2 i.data
                                  ; incr count
                                  ])
                        ] )
                    ; ( n 1
                      , emit_unless_aborted
                          [ incr count; hw.txd <-- select buffer.value ~high:1 ~low:0 ] )
                    ; ( n 2
                      , emit_unless_aborted
                          [ incr count; hw.txd <-- select buffer.value ~high:3 ~low:2 ] )
                    ; ( n 3
                      , emit_unless_aborted
                          [ count <--. 0; hw.txd <-- select buffer.value ~high:5 ~low:4 ]
                      )
                    ]
                ] )
            ; ( CRC
              , [ hw.txen <-- vdd
                ; shift_out_crc ()
                ; if_
                    (count.value <>:. (32 / 2) - 2)
                    [ incr count ]
                    [ count <--. 0; state.set_next IPG ]
                ] )
            ; ( IPG
              , [ if_
                    (count.value ==:. ipg_cycles - 1)
                    [ count <--. 0; state.set_next Ready ]
                  @@ else_ [ incr count ]
                ] )
            ]
        ]);
    let%hw ready_to_start = state.is Ready in
    let%hw sfd_sent = state.is Data in
    let%hw ready_for_data = state.is Data &: (count.value ==:. 0) in
    { hw = Hw.Tx.Of_always.value hw; ready_to_start; ready_for_data; sfd_sent }
  ;;

  let name = "rmii_tx_ll"

  include functor Component.Make
end

module Arbitrer = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; data : 'a [@bits 8]
      ; last_header_byte : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { hw : 'a Hw.Tx.t
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end
end

module Tx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx : 'a Packet_stream.t
      ; last_header_byte : 'a
      }
    [@@deriving hardcaml]
  end

  module Lifecycle_manager = struct
    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; ready_for_data : 'a
        ; stop : 'a
        ; abort : 'a
        ; enable_crc : 'a
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { stop_abort : 'a
        ; stop_with_crc : 'a
        }
      [@@deriving hardcaml]
    end

    module State = struct
      type t =
        | Pre_crc
        | Post_crc
        | Clean_stopping
        | Clean_aborting
      [@@deriving sexp_of, compare ~localize, enumerate]
    end

    let create scope (i : _ I.t) : _ O.t =
      let reg_spec = Clocking.to_spec i.clocking in
      let%hw.Always.State_machine state =
        Always.State_machine.create (module State) reg_spec
      in
      let%hw_var stop_abort = Always.Variable.wire ~default:gnd () in
      let%hw_var stop_with_crc = Always.Variable.wire ~default:gnd () in
      Always.(
        let clean_abort =
          proc [ stop_abort <-- vdd; stop_with_crc <-- vdd; state.set_next Pre_crc ]
        in
        let clean_stop = proc [ stop_with_crc <-- vdd; state.set_next Pre_crc ] in
        compile
          [ state.switch
              [ ( Pre_crc
                , [ if_ (i.stop |: i.abort) [ stop_abort <-- vdd ]
                    @@ else_ [ when_ i.enable_crc [ state.set_next Post_crc ] ]
                  ] )
              ; ( Post_crc
                , [ if_
                      i.abort
                      [ if_ i.ready_for_data [ clean_abort ]
                        @@ else_ [ state.set_next Clean_aborting ]
                      ]
                    @@ elif
                         i.stop
                         [ if_ i.ready_for_data [ clean_stop ]
                           @@ else_ [ state.set_next Clean_stopping ]
                         ]
                    @@ else_ []
                  ] )
              ; Clean_stopping, [ when_ i.ready_for_data [ clean_stop ] ]
              ; Clean_aborting, [ when_ i.ready_for_data [ clean_abort ] ]
              ]
          ]);
      { stop_abort = stop_abort.value; stop_with_crc = stop_with_crc.value }
    ;;
  end

  module O = struct
    type 'a t =
      { hw : 'a Hw.Tx.t
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Waiting
      | Queued_start
      | Emitting
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw_var start = Always.Variable.wire ~default:gnd () in
    let%hw_var stop = Always.Variable.wire ~default:gnd () in
    let%hw_var abort = Always.Variable.wire ~default:gnd () in
    let%hw_var ready = Always.Variable.wire ~default:gnd () in
    let ready_for_data' = wire 1 in
    let%hw enable_crc = state.is Emitting &: i.last_header_byte in
    let%tydi { stop_abort; stop_with_crc } =
      Lifecycle_manager.create
        (Scope.sub_scope scope "lifecycle_manager")
        { clocking = i.clocking
        ; ready_for_data = ready_for_data'
        ; stop = stop.value
        ; abort = abort.value
        ; enable_crc
        }
    in
    let%tydi { hw; ready_to_start; ready_for_data; sfd_sent = _ } =
      Tx_ll.hierarchical
        scope
        { clocking = i.clocking
        ; data = i.tx.data
        ; start = start.value
        ; stop_abort
        ; stop_with_crc
        }
    in
    assign ready_for_data' ready_for_data;
    Always.(
      compile
        [ state.switch
            [ ( Waiting
              , [ when_
                    i.tx.start
                    [ if_ ready_to_start [ start <-- vdd; state.set_next Emitting ]
                      @@ else_ [ state.set_next Queued_start ]
                    ]
                ] )
            ; ( Queued_start
              , [ if_ i.tx.abort [ state.set_next Waiting ]
                  @@ else_
                       [ when_ ready_to_start [ start <-- vdd; state.set_next Emitting ] ]
                ] )
            ; ( Emitting
              , [ ready <-- ready_for_data
                ; if_ i.tx.abort [ abort <-- vdd; state.set_next Waiting ]
                  @@ elif i.tx.stop [ stop <-- vdd; state.set_next Waiting ]
                  @@ else_ []
                ] )
            ]
        ]);
    { hw; ready = ready.value }
  ;;

  let name = "rmii_tx"

  include functor Component.Make
end
