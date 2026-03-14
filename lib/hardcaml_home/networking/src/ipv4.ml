open! Core
open! Hardcaml
open! Hardcaml_home
open! Signal
open! Signal_extended

let ethertype = 0x0800 |> Bits.of_unsigned_int ~width:16

module Header = struct
  type 'a t =
    { version : 'a [@bits 4]
    ; ihl : 'a [@bits 4]
    ; dscp : 'a [@bits 6]
    ; ecn : 'a [@bits 2]
    ; length : 'a [@bits 16]
    ; identification : 'a [@bits 16]
    ; flags : 'a [@bits 3]
    ; fragment_offset : 'a [@bits 13]
    ; ttl : 'a [@bits 8]
    ; protocol : 'a [@bits 8]
    ; checksum : 'a [@bits 16]
    ; src : 'a Addr.Ip.t
    ; dst : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]
end

module Header_chunks = struct
  module Version_etc = struct
    type 'a t =
      { version : 'a [@bits 4]
      ; ihl : 'a [@bits 4]
      ; dscp : 'a [@bits 6]
      ; ecn : 'a [@bits 2]
      ; length : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end

  module Id_fragment = struct
    type 'a t =
      { identification : 'a [@bits 16]
      ; flags : 'a [@bits 3]
      ; fragment_offset : 'a [@bits 13]
      }
    [@@deriving hardcaml]
  end

  module Ttl_protocol_checksum = struct
    type 'a t =
      { ttl : 'a [@bits 8]
      ; protocol : 'a [@bits 8]
      ; checksum : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end
end

module Mini_header = struct
  type 'a t =
    { payload_length : 'a [@bits 16]
    ; protocol : 'a [@bits 8]
    ; src : 'a Addr.Ip.t
    ; dst : 'a Addr.Ip.t
    }
  [@@deriving hardcaml]

  let header_template () =
    Header.Of_signal.of_unsigned_ints
      { version = 4
      ; ihl = 5
      ; dscp = 0
      ; ecn = 0
      ; length = 0
      ; identification = 0
      ; flags = 0b010
      ; fragment_offset = 0
      ; ttl = 64
      ; protocol = 0
      ; checksum = 0
      ; src = Addr.Ip.const 0
      ; dst = Addr.Ip.const 0
      }
  ;;

  let to_header_without_checksum { payload_length; protocol; src; dst } =
    { (header_template ()) with
      length = payload_length +:. (Header.sum_of_port_widths / 8)
    ; protocol
    ; src
    ; dst
    }
  ;;
end

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
      { header : 'a Mini_header.t
      ; payload : 'a Packet_stream.t
      ; valid : 'a
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Wait_start
      | Version_length
      | Id_fragmentation
      | Ttl_protocol_checksum
      | Src_ip
      | Dst_ip
      | Payload
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Clocking.to_spec i.clocking in
    let%hw.Always.State_machine state =
      Always.State_machine.create (module State) reg_spec
    in
    let%hw.Mini_header.Of_always header = Mini_header.Of_always.reg reg_spec in
    let%hw_var buffer = Always.Variable.reg ~width:(3 * 8) reg_spec in
    let%hw_var step = Always.Variable.reg ~width:4 reg_spec in
    let%hw read_value = buffer.value @: i.rx.data in
    let%hw_var payload_count =
      Always.Variable.reg ~width:Header.port_widths.length reg_spec
    in
    let%hw_var checksum_n = Always.Variable.reg ~width:16 reg_spec in
    let payload, ~valid =
      Always.(
        let shift_into buf = Always_extended.shift_into_var buf ~add:i.rx.data ~at:`lsb in
        Packet_stream.Always_helper.compile_with
          scope
          ~reg_spec
          ~upstream_abort:i.rx.abort
          ~reset:
            [ state.set_next Wait_start
            ; step <--. 0
            ; payload_count <--. 0
            ; checksum_n <--. 0
            ]
        @@ fun payload ->
        let handle_field =
          state.switch
            [ ( Version_length
              , let%tydi { version; ihl; dscp = _; ecn = _; length } =
                  Header_chunks.Version_etc.Of_signal.unpack read_value ~rev:true
                in
                [ header.payload_length <-- length -:. (5 * 4)
                ; state.set_next Id_fragmentation
                ; when_ (version <>:. 4 |: (ihl <>:. 5)) [ payload.abort ]
                ] )
            ; ( Id_fragmentation
              , let%tydi { identification = _; flags; fragment_offset } =
                  Header_chunks.Id_fragment.Of_signal.unpack read_value ~rev:true
                in
                [ state.set_next Ttl_protocol_checksum
                ; when_ (fragment_offset <>:. 0 |: flags.:(0)) [ payload.abort ]
                ] )
            ; ( Ttl_protocol_checksum
              , let%tydi { ttl = _; protocol; checksum = _ } =
                  Header_chunks.Ttl_protocol_checksum.Of_signal.unpack
                    read_value
                    ~rev:true
                in
                [ header.protocol <-- protocol; state.set_next Src_ip ] )
            ; ( Src_ip
              , [ Addr.Ip.Of_always.assign
                    header.src
                    (Addr.Ip.Of_signal.unpack read_value ~rev:true)
                ; state.set_next Dst_ip
                ] )
            ; ( Dst_ip
              , [ Addr.Ip.Of_always.assign
                    header.dst
                    (Addr.Ip.Of_signal.unpack read_value ~rev:true)
                  (* check 0 length packet *)
                ; if_
                    i.rx.stop
                    [ if_
                        (header.payload_length.value ==:. 0)
                        [ payload.stop_if_started; payload.reset ]
                        [ payload.abort ]
                    ]
                  @@ else_ [ state.set_next Payload ]
                ] )
            ; Wait_start, [ (* impossible *) ]
            ; Payload, [ (* impossible *) ]
            ]
        in
        [ state.switch
            [ ( Wait_start
              , [ when_
                    (i.rx.start &: (i.ethertype ==: of_bits ethertype))
                    [ state.set_next Version_length ]
                ] )
            ; ( Payload
              , let%hw_var next_count =
                  Always.Variable.wire ~default:payload_count.value ()
                in
                [ payload.ensure_started
                ; unless (all_bits_set checksum_n.value) [ payload.abort ]
                ; payload_count <-- next_count.value
                ; when_
                    i.rx_valid
                    [ next_count <-- Signal.incr payload_count.value
                    ; when_
                        (next_count.value <=: header.payload_length.value)
                        [ payload.emit i.rx.data ]
                    ]
                ; when_
                    i.rx.stop
                    [ if_
                        (next_count.value >=: header.payload_length.value)
                        [ payload.stop_if_started; payload.reset ]
                      @@ else_ [ payload.abort ]
                    ]
                ] )
            ]
            ~default:
              [ when_
                  i.rx_valid
                  [ when_
                      step.value.:(0)
                      [ checksum_n
                        <-- ones_complement_add
                              checksum_n.value
                              (sel_bottom buffer.value ~width:8 @: i.rx.data)
                      ]
                  ; if_
                      (step.value ==:. 3)
                      [ step <--. 0; handle_field ]
                      [ incr step; shift_into buffer ]
                  ; when_
                      (i.rx.stop &: ~:(state.is Dst_ip &: (step.value ==:. 3)))
                      [ payload.abort ]
                  ]
              ]
        ])
    in
    { header = Mini_header.Of_always.value header; payload; valid }
  ;;

  let name = "ipv4_rx"

  include functor Component.Make
end

module Tx = struct
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
      }
    [@@deriving hardcaml]
  end

  (* worst case (data needed immediately after start cycle)

  0: start called -- header stable
  1: version/ihl out
  2: dscp/ecn out
  3: total_length 0
  4: total_length 1
  5: id 0
  6: id 1
  7: flag/frag 0
  8: flag/frag 1
  9: ttl
  10: protocol

  <header checksum must be ready here>

  There are 6 non-constant fields. So It's sufficient to just start sending and
  know the checksum will be ready in time if we cached

  But actually, we can get through everything in time if we just calculate the
  checksum twice as fast as we output bytes: it'll be definitely ready on the
  same cycle *)

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
    let%hw_var checksum_n = Always.Variable.reg ~width:16 reg_spec in
    let send_parts =
      { i.header with checksum = ~:(checksum_n.value) }
      |> Header.Of_signal.pack ~rev:true
      |> split_msb ~exact:true ~part_width:8
    in
    let max_count = List.length send_parts - 1 in
    let%hw_var count =
      Always.Variable.reg ~width:(num_bits_to_represent max_count) reg_spec
    in
    let init_checksum, checksum_add_words =
      let checksum_parts =
        Header.Of_signal.pack i.header ~rev:true |> split_msb ~exact:true ~part_width:16
      in
      let first_word = List.hd_exn checksum_parts in
      let to_add =
        List.filteri checksum_parts ~f:(fun idx _ ->
          match idx with
          | 0 -> false (* init checksum*)
          | 5 -> false (* checksum itself, 0 when computing *)
          | _ -> true)
      in
      first_word, to_add
    in
    let num_checksum_parts = List.length checksum_add_words in
    let%hw.Packet_stream.Of_always tx = Packet_stream.Of_always.wire zero in
    let%hw_var payload_ready = Always.Variable.wire ~default:gnd () in
    Always.(
      compile
        [ state.switch
            [ ( Waiting
              , [ when_
                    i.payload.start
                    [ tx.start <-- vdd
                    ; state.set_next Header
                    ; count <--. 0
                    ; checksum_n <-- init_checksum
                    ]
                ] )
            ; ( Header
              , [ if_ i.payload.abort [ tx.abort <-- vdd; state.set_next Waiting ]
                  @@ else_
                       [ when_
                           i.tx_ready
                           [ when_
                               (count.value <:. num_checksum_parts)
                               [ checksum_n
                                 <-- ones_complement_add
                                       checksum_n.value
                                       (mux count.value checksum_add_words)
                               ]
                           ; tx.data <-- mux count.value send_parts
                           ; if_
                               (count.value ==:. max_count)
                               [ state.set_next Payload ]
                               [ incr count ]
                           ]
                       ]
                ] )
            ; ( Payload
              , [ if_ i.payload.abort [ tx.abort <-- vdd; state.set_next Waiting ]
                  @@ else_
                       [ payload_ready <-- i.tx_ready
                       ; tx.data <-- i.payload.data
                       ; tx.stop <-- i.payload.stop
                       ; when_ i.payload.stop [ state.set_next Waiting ]
                       ]
                ] )
            ]
        ]);
    { tx = Packet_stream.Of_always.value tx; payload_ready = payload_ready.value }
  ;;

  let name = "ipv4_tx"

  include functor Component.Make
end
