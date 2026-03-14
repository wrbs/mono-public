open! Core
open! Hardcaml
open Signal

module Make (Header : Interface.S) = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; tx_ready : 'a
      ; header : 'a Header.t
      ; payload : 'a Packet_stream.t
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { tx : 'a Packet_stream.t
      ; payload_ready : 'a
      ; last_header_byte : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Waiting
      | Header
      | Payload
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec (Clocking.add_clear i.clocking i.payload.abort) in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let send_parts =
      i.header |> Header.Of_signal.pack ~rev:true |> split_msb ~exact:true ~part_width:8
    in
    let%hw.Packet_stream.Of_always tx = Packet_stream.Of_always.wire zero in
    let%hw_var payload_ready = Always.Variable.wire ~default:gnd () in
    let%hw_var last_header_byte = Always.Variable.wire ~default:gnd () in
    let max_count = List.length send_parts - 1 in
    let%hw_var count =
      Always.Variable.reg ~width:(num_bits_to_represent max_count) reg_spec
    in
    Always.(
      compile
        [ state.switch
            [ ( Waiting
              , [ when_
                    i.payload.start
                    [ tx.start <-- vdd; state.set_next Header; count <--. 0 ]
                ] )
            ; ( Header
              , [ if_ i.payload.abort [ tx.abort <-- vdd; state.set_next Waiting ]
                  @@ else_
                       [ when_
                           i.tx_ready
                           [ tx.data <-- mux count.value send_parts
                           ; if_
                               (count.value ==:. max_count)
                               [ state.set_next Payload; last_header_byte <-- vdd ]
                               [ incr count ]
                           ]
                       ]
                ] )
            ; ( Payload
              , [ if_ i.payload.abort [ tx.abort <-- vdd; state.set_next Waiting ]
                  @@ else_
                       [ when_
                           i.tx_ready
                           [ payload_ready <-- vdd
                           ; tx.data <-- i.payload.data
                           ; tx.stop <-- i.payload.stop
                           ; when_ i.payload.stop [ state.set_next Waiting ]
                           ]
                       ]
                ] )
            ]
        ]);
    { tx = Packet_stream.Of_always.value tx
    ; payload_ready = payload_ready.value
    ; last_header_byte = last_header_byte.value
    }
  ;;
end
