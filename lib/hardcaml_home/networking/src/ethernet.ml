open! Core
open! Hardcaml
open! Hardcaml_home
open! Signal
open! Signal_extended

module Header = struct
  type 'a t =
    { dst : 'a Addr.Mac.t
    ; src : 'a Addr.Mac.t
    ; ethertype : 'a [@bits 16] (* note: doesn't support .1q *)
    }
  [@@deriving hardcaml]
end

module Rx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    (* header outputs valid on payload start, until the next cycle after payload stop *)
    type 'a t =
      { header : 'a Header.t
      ; payload : 'a Packet_stream.t
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Idle
      | Header
      | Payload_buffer
      | Payload
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw_var header_buffer =
      Always.Variable.reg ~width:Header.sum_of_port_widths reg_spec
    in
    let%hw_var data_buffer = Always.Variable.reg ~width:32 reg_spec in
    let header_bytes = Header.sum_of_port_widths / 8 in
    let max_count = header_bytes - 1 in
    let count_bits = num_bits_to_represent max_count in
    let%hw_var count = Always.Variable.reg ~width:count_bits reg_spec in
    let payload, ~valid =
      Always.(
        let shift_into buf = buf <-- shift_into buf.value ~add:i.rx.data ~at:`lsb in
        Packet_stream.Always_helper.compile_with
          scope
          ~reg_spec
          ~upstream_abort:i.rx.abort
          ~reset:[ count <--. 0; state.set_next Idle ]
        @@ fun payload ->
        [ state.switch
            [ Idle, [ when_ i.rx.start [ count <--. 0; state.set_next Header ] ]
            ; ( Header
              , [ when_
                    i.rx_valid
                    [ shift_into header_buffer
                    ; if_
                        (count.value ==:. max_count)
                        [ count <--. 0; state.set_next Payload_buffer ]
                      @@ else_ [ incr count ]
                    ]
                ] )
            ; ( Payload_buffer
              , [ payload.ensure_started
                ; when_
                    i.rx_valid
                    [ shift_into data_buffer
                    ; if_ (count.value ==:. 3) [ state.set_next Payload ]
                      @@ else_ [ incr count ]
                    ]
                ] )
            ; ( Payload
              , [ when_
                    i.rx_valid
                    [ shift_into data_buffer
                    ; payload.emit (sel_top data_buffer.value ~width:8)
                    ]
                ] )
            ]
        ; when_ i.rx.stop [ payload.stop_if_started; payload.reset ]
        ])
    in
    { header = Header.Of_signal.unpack ~rev:true header_buffer.value; payload; valid }
  ;;

  let name = "ethernet_rx"

  include functor Component.Make
end

module Padder = struct
  let min_bytes = 46

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; padded_ready : 'a
      ; unpadded : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    (* header outputs valid on payload start, until the next cycle after payload stop *)
    type 'a t =
      { padded : 'a Packet_stream.t
      ; unpadded_ready : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Wait_start
      | Emitting
      | Flushing
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create (scope : Scope.t) (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let unpadded, ~emitting:_ =
      Packet_stream.start_buffer
        scope
        i.unpadded
        ~ready_to_start:(state.is Wait_start)
        ~ready_for_data:i.padded_ready
        ~reg_spec
    in
    let%hw_var bits_left =
      Always.Variable.reg reg_spec ~width:(num_bits_to_represent min_bytes)
    in
    let%hw emitted_enough = bits_left.value ==:. 0 in
    let%hw.Packet_stream.Of_always padded = Packet_stream.Of_always.wire zero in
    let%hw_var unpadded_ready = Always.Variable.wire ~default:gnd () in
    Always.(
      compile
        [ state.switch
            [ ( Wait_start
              , [ when_
                    unpadded.start
                    [ padded.start <-- vdd
                    ; state.set_next Emitting
                    ; bits_left <--. min_bytes
                    ]
                ] )
            ; ( Emitting
              , [ if_ unpadded.abort [ padded.abort <-- vdd; state.set_next Wait_start ]
                  @@ else_
                       [ when_
                           i.padded_ready
                           [ unpadded_ready <-- vdd
                           ; if_
                               unpadded.stop
                               [ if_
                                   emitted_enough
                                   [ padded.stop <-- vdd; state.set_next Wait_start ]
                                 @@ else_ [ state.set_next Flushing; decr bits_left ]
                               ]
                             @@ else_
                                  [ when_ ~:emitted_enough [ decr bits_left ]
                                  ; padded.data <-- unpadded.data
                                  ]
                           ]
                       ]
                ] )
            ; ( Flushing
              , [ when_
                    i.padded_ready
                    [ if_
                        emitted_enough
                        [ padded.stop <-- vdd; state.set_next Wait_start ]
                      @@ else_ [ decr bits_left ]
                    ]
                ] )
            ]
        ]);
    { padded = Packet_stream.Of_always.value padded
    ; unpadded_ready = unpadded_ready.value
    }
  ;;
end

module Tx = struct
  module Helper = Tx_helper.Make (Header)
  module I = Helper.I
  module O = Helper.O

  let create scope (i : _ I.t) : _ O.t =
    let padded_ready = wire 1 in
    let%tydi { padded; unpadded_ready } =
      Padder.create
        (Scope.sub_scope scope "padder")
        { clocking = i.clocking; padded_ready; unpadded = i.payload }
    in
    let%tydi { tx; payload_ready; last_header_byte } =
      Helper.create
        scope
        { clocking = i.clocking
        ; tx_ready = i.tx_ready
        ; payload = padded
        ; header = i.header
        }
    in
    assign padded_ready payload_ready;
    { tx; payload_ready = unpadded_ready; last_header_byte }
  ;;

  let name = "ethernet_tx"

  include functor Component.Make
end
