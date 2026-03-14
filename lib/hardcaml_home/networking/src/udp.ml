open! Core
open! Hardcaml_home
open! Signal

let ip_protocol = 17 |> Bits.of_unsigned_int ~width:8

module Ports = struct
  type 'a t =
    { src : 'a [@bits 16]
    ; dst : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module Header = struct
  type 'a t =
    { ports : 'a Ports.t
    ; length : 'a [@bits 16]
    ; checksum : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module Rx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      ; ip_length : 'a [@bits 16]
      ; ip_protocol : 'a [@bits 8]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { ports : 'a Ports.t
      ; payload_length : 'a [@bits 16]
      ; payload : 'a Packet_stream.t
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Wait_start
      | Src
      | Dest
      | Length
      | Checksum
      | Payload
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw.Ports.Of_always ports = Ports.Of_always.reg reg_spec in
    let%hw_var buffer = Always.Variable.reg ~width:8 reg_spec in
    let%hw_var second_byte = Always.Variable.reg ~width:1 reg_spec in
    let%hw length_valid = state.is Checksum |: state.is Payload in
    let%hw payload_length = mux2 length_valid (i.ip_length -:. 8) (zero 16) in
    let%hw cur_word = buffer.value @: i.rx.data in
    let payload, ~valid =
      Always.(
        Packet_stream.Always_helper.compile_with
          scope
          ~reg_spec
          ~upstream_abort:i.rx.abort
          ~reset:[ state.set_next Wait_start; second_byte <-- vdd ]
        @@
        let reader then_ =
          when_
            i.rx_valid
            [ second_byte <-- ~:(second_byte.value)
            ; if_ second_byte.value then_ [ buffer <-- i.rx.data ]
            ]
        in
        fun payload ->
          [ state.switch
              [ ( Wait_start
                , [ when_
                      (i.rx.start &: (i.ip_protocol ==: of_bits ip_protocol))
                      [ state.set_next Src ]
                  ] )
              ; Src, [ reader [ ports.src <-- cur_word; state.set_next Dest ] ]
              ; Dest, [ reader [ ports.dst <-- cur_word; state.set_next Length ] ]
              ; ( Length
                , [ reader
                      [ if_
                          (cur_word ==: i.ip_length)
                          [ state.set_next Checksum ]
                          [ payload.abort ]
                      ]
                  ] )
              ; Checksum, [ payload.ensure_started; reader [ state.set_next Payload ] ]
              ; ( Payload
                , [ when_ i.rx_valid [ payload.emit i.rx.data ]
                  ; when_ i.rx.stop [ payload.stop_if_started ]
                  ] )
              ]
          ])
    in
    { ports = Ports.Of_always.value ports; payload_length; payload; valid }
  ;;

  let name = "udp_rx"

  include functor Component.Make
end

module Tx = struct
  module Helper = Tx_helper.Make (Header)

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx_ready : 'a
      ; ports : 'a Ports.t
      ; payload_length : 'a [@bits 16]
      ; payload : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx : 'a Packet_stream.t
      ; payload_ready : 'a
      ; length : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let%hw length = i.payload_length +:. (Header.sum_of_port_widths / 8) in
    let%hw.Header.Of_signal header = { ports = i.ports; length; checksum = zero 16 } in
    let%tydi { tx; payload_ready; _ } =
      Helper.create
        scope
        { clocking = i.clocking; tx_ready = i.tx_ready; header; payload = i.payload }
    in
    { tx; payload_ready; length = i.payload_length +:. (Header.sum_of_port_widths / 8) }
  ;;

  let name = "udp_tx"

  include functor Component.Make
end
