open! Core
open! Hardcaml
open! Signal

open struct
  open Hardcaml_hobby_boards
  module Axi32 = Ethernet.Axi32
  module Ethernet = Ethernet
  module Ethernet_types = Ethernet_types
end

module Rx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; eth : 'a Ethernet.Rx.O.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a [@bits 8]
      ; valid : 'a
      ; last : 'a
      ; rx_error : 'a
      }
  end

  module State = struct
    type t =
      | Idle
      | B1
      | B2
      | B3
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let rx = i.eth.axi_tx in
    let%hw_var buffer = Always.Variable.reg ~width:(3 * 8) reg_spec in
    let%hw_var last_word = Always.Variable.reg ~width:1 reg_spec in
    let%hw_var last_byte = Always.Variable.wire () ~default:gnd in
    let%hw_var data = Always.Variable.wire () ~default:(sel_top buffer.value ~width:8) in
    let%hw_var valid = Always.Variable.wire () ~default:gnd in
    let state = Always.State_machine.create (module State) reg_spec in
    Always.(
      compile
        [ buffer <-- sll buffer.value ~by:8
        ; state.switch
            [ ( Idle
              , [ when_
                    rx.tvalid
                    [ data <-- sel_top rx.tdata ~width:8
                    ; buffer <-- drop_top rx.tdata ~width:8
                    ; valid <-- vdd
                    ; switch
                        rx.tkeep
                        [ ( of_bit_string "1111"
                          , [ state.set_next B3; last_word <-- rx.tlast ] )
                        ; ( of_bit_string "1110"
                          , [ state.set_next B2; last_word <-- rx.tlast ] )
                        ; ( of_bit_string "1100"
                          , [ state.set_next B1; last_word <-- rx.tlast ] )
                        ; of_bit_string "1000", [ last_byte <-- rx.tlast ]
                        ]
                    ]
                ] )
            ; ( B1
              , [ valid <-- vdd
                ; state.set_next Idle
                ; when_ last_word.value [ last_byte <-- vdd ]
                ] )
            ; B2, [ valid <-- vdd; state.set_next B1 ]
            ; B3, [ valid <-- vdd; state.set_next B2 ]
            ]
        ; when_ i.eth.rx_error [ state.set_next Idle; last_word <-- gnd ]
        ]);
    { data = data.value
    ; valid = valid.value
    ; last = last_byte.value
    ; rx_error = i.eth.rx_error
    }
  ;;
end
