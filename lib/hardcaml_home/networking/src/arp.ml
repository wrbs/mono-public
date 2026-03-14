open! Core
open! Hardcaml_home
open! Signal
open! Signal_extended

let ethertype = Bits.of_unsigned_int 0x0806 ~width:16

module Oper = struct
  let request = Bits.of_unsigned_int 1 ~width:16
  let reply = Bits.of_unsigned_int 2 ~width:16
end

module Fields = struct
  type 'a t =
    { oper : 'a [@bits 16]
    ; sha : 'a Addr.Mac.t
    ; spa : 'a Addr.Ip.t
    ; tha : 'a Addr.Mac.t
    ; tpa : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]
end

let header_bytes =
  concat_msb
    [ (* htype *)
      of_unsigned_int 1 ~width:16
    ; (* ptype *)
      of_unsigned_int 0x800 ~width:16
    ; (* hlen *)
      of_unsigned_int 6 ~width:8
    ; (* plen *)
      of_unsigned_int 4 ~width:8
    ]
  |> split_msb ~exact:true ~part_width:8
;;

let max_header_count = List.length header_bytes - 1
let max_fields_count = (Fields.sum_of_port_widths / 8) - 1
let count_bits = num_bits_to_represent (max max_header_count max_fields_count)

module Rx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; rx : 'a Packet_stream.t
      ; rx_valid : 'a
      ; ethertype : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { fields : 'a Fields.t
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Wait_start
      | Header
      | Payload
      | Padding
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw_var count = Always.Variable.reg ~width:count_bits reg_spec in
    let%hw_var fields_buffer =
      Always.Variable.reg ~width:Fields.sum_of_port_widths reg_spec
    in
    let%hw_var next_fields = Always.Variable.wire ~default:fields_buffer.value () in
    let%hw_var valid = Always.Variable.cut_through_reg ~width:1 reg_spec in
    Always.(
      let reset = proc [ state.set_next Wait_start; count <--. 0 ] in
      compile
        [ fields_buffer <-- next_fields.value
        ; state.switch
            [ ( Wait_start
              , [ when_
                    (i.rx.start &: (i.ethertype ==: of_bits ethertype))
                    [ state.set_next Header; count <--. 0 ]
                ] )
            ; ( Header
              , [ if_ (i.rx.stop |: i.rx.abort) [ reset ]
                  @@ else_
                       [ when_
                           i.rx_valid
                           [ (let expected = mux count.value header_bytes in
                              if_
                                (i.rx.data ==: expected)
                                [ if_
                                    (count.value ==:. max_header_count)
                                    [ state.set_next Payload
                                    ; count <--. 0
                                    ; valid <-- gnd
                                    ]
                                  @@ else_ [ incr count ]
                                ]
                              @@ else_ [ reset ])
                           ]
                       ]
                ] )
            ; ( Payload
              , [ if_ i.rx.abort [ reset ]
                  @@ elif
                       i.rx_valid
                       [ next_fields
                         <-- shift_into fields_buffer.value ~add:i.rx.data ~at:`lsb
                       ; if_
                           (count.value ==:. max_fields_count)
                           [ if_ i.rx.stop [ valid <-- vdd; reset ]
                             @@ else_ [ state.set_next Padding ]
                           ]
                         @@ else_ [ incr count; when_ i.rx.stop [ reset ] ]
                       ]
                  @@ else_ [ when_ i.rx.stop [ reset ] ]
                ] )
            ; ( Padding
              , [ if_ i.rx.abort [ reset ]
                  @@ else_ [ when_ i.rx.stop [ valid <-- vdd; reset ] ]
                ] )
            ]
        ]);
    let%hw.Fields.Of_signal fields =
      Fields.Of_signal.unpack ~rev:true next_fields.value
    in
    { fields; valid = valid.value }
  ;;

  let name = "arp_rx"

  include functor Component.Make
end

module Tx = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx_ready : 'a
      ; fields : 'a Fields.t
      ; start : 'a
      ; abort : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx : 'a Packet_stream.t
      ; ready_to_start : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Wait_start
      | Header
      | Payload
      | Stop
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw_var count = Always.Variable.reg ~width:count_bits reg_spec in
    let%hw.Packet_stream.Of_always tx = Packet_stream.Of_always.wire zero in
    let field_bytes =
      Fields.Of_signal.pack ~rev:true i.fields |> split_msb ~exact:true ~part_width:8
    in
    Always.(
      let reset = proc [ state.set_next Wait_start; count <--. 0 ] in
      compile
        [ state.switch
            [ ( Wait_start
              , [ when_ i.start [ tx.start <-- vdd; state.set_next Header; count <--. 0 ]
                ] )
            ; ( Header
              , [ if_ i.abort [ tx.abort <-- vdd; reset ]
                  @@ else_
                       [ when_
                           i.tx_ready
                           [ tx.data <-- mux count.value header_bytes
                           ; if_
                               (count.value ==:. max_header_count)
                               [ state.set_next Payload; count <--. 0 ]
                             @@ else_ [ incr count ]
                           ]
                       ]
                ] )
            ; ( Payload
              , [ if_ i.abort [ tx.abort <-- vdd; reset ]
                  @@ else_
                       [ when_
                           i.tx_ready
                           [ tx.data <-- mux count.value field_bytes
                           ; if_
                               (count.value ==:. max_fields_count)
                               [ state.set_next Stop ]
                             @@ else_ [ incr count ]
                           ]
                       ]
                ] )
            ; Stop, [ when_ i.tx_ready [ tx.stop <-- vdd; reset ] ]
            ]
        ]);
    { tx = Packet_stream.Of_always.value tx; ready_to_start = state.is Wait_start }
  ;;

  let name = "arp_tx"

  include functor Component.Make
end
