open! Core
open! Hardcaml
open! Signal
open Helpers

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; uart : 'a Byte_stream.Rx.t
    ; init_partial_checksum : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { conf : 'a Network_conf.t
    ; configured : 'a
    ; uart : 'a Byte_stream.Tx.t
    ; partial_checksum : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module State = struct
  type t =
    | Waiting_start
    | Reading
    | Configured
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let conf_bits = Network_conf.sum_of_port_widths
let read_bytes = conf_bits / 8
let read_bytes_bits = num_bits_to_represent read_bytes

let create scope (i : _ I.t) : _ O.t =
  let reg_spec = Clocking.to_spec i.clocking in
  let%hw.Always.State_machine state =
    Always.State_machine.create (module State) reg_spec
  in
  let%hw_var cur_byte = Always.Variable.reg ~width:read_bytes_bits reg_spec in
  let%hw_var conf_data = Always.Variable.reg ~width:conf_bits reg_spec in
  let%hw_var next_conf = Always.Variable.wire ~default:conf_data.value () in
  let%hw_var sent_ack = Always.Variable.reg ~width:1 reg_spec in
  let%hw_var tx_valid = Always.Variable.wire ~default:gnd () in
  let%hw_var configured = Always.Variable.wire ~default:gnd () in
  let%hw_var partial_checksum = Always.Variable.reg ~width:16 reg_spec in
  let add_to_checksum =
    [ 1; 3; 7; 9 ]
    |> List.map ~f:(fun n -> cur_byte.value ==:. n)
    |> List.reduce_exn ~f:( |: )
  in
  Always.(
    compile
      [ conf_data <-- next_conf.value
      ; state.switch
          [ ( Waiting_start
            , [ when_
                  (i.uart.rx_valid &: i.uart.rx_data ==:. Char.to_int '!')
                  [ state.set_next Reading; partial_checksum <-- i.init_partial_checksum ]
              ] )
          ; ( Reading
            , [ when_
                  i.uart.rx_valid
                  [ next_conf <-- drop_top conf_data.value ~width:8 @: i.uart.rx_data
                  ; when_
                      add_to_checksum
                      [ (let to_add =
                           sel_bottom conf_data.value ~width:8 @: i.uart.rx_data
                         in
                         partial_checksum
                         <-- ones_complement_add partial_checksum.value to_add)
                      ]
                  ; if_
                      (cur_byte.value ==:. read_bytes - 1)
                      [ configured <-- vdd; state.set_next Configured ]
                      [ incr cur_byte ]
                  ]
              ] )
          ; ( Configured
            , [ configured <-- vdd
              ; when_
                  (~:(sent_ack.value) &: i.uart.tx_ready)
                  [ tx_valid <-- vdd; sent_ack <-- vdd ]
              ] )
          ]
      ]);
  let%hw.Network_conf.Of_signal conf = Network_conf.Of_signal.unpack next_conf.value in
  { conf
  ; configured = configured.value
  ; uart = { tx_data = of_char 'K'; tx_valid = tx_valid.value }
  ; partial_checksum = partial_checksum.value
  }
;;
