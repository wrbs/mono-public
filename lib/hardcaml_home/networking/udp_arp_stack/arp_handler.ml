open! Core
open! Hardcaml_home
open! Hardcaml_home_networking
open! Signal

let broadcast_mac =
  Addr.Mac.Const.of_string "ff:ff:ff:ff:ff:ff" |> Addr.Mac.map ~f:of_bits
;;

let ip_equal a b = Addr.Ip.Of_signal.pack a ==: Addr.Ip.Of_signal.pack b

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; rx : 'a Packet_stream.t
    ; rx_valid : 'a
    ; ethertype : 'a [@bits 16]
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { tx : 'a Packet_stream.t
    ; dst_mac : 'a Addr.Mac.t
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let clocking = i.clocking in
  let reg_spec = Clocking.to_spec clocking in
  let%hw_var is_init = Always.Variable.reg ~width:1 reg_spec in
  let%hw.Addr.Mac.Of_always dst_mac = Addr.Mac.Of_always.cut_through_reg reg_spec in
  let%hw.Arp.Fields.Of_always send_fields =
    Arp.Fields.Of_always.cut_through_reg reg_spec
  in
  let%hw_var start_send = Always.Variable.wire ~default:gnd () in
  let arp_rx =
    Arp.Rx.hierarchical
      scope
      { clocking; rx = i.rx; rx_valid = i.rx_valid; ethertype = i.ethertype }
  in
  let valid_request_pending =
    arp_rx.valid
    &: (arp_rx.fields.oper ==: of_bits Arp.Oper.request)
    &: ip_equal arp_rx.fields.tpa i.fpga_ip
  in
  let%hw_var cur_handled = Always.Variable.reg ~width:1 reg_spec in
  let arp_tx =
    Arp.Tx.hierarchical
      scope
      { clocking
      ; tx_ready = i.tx_ready
      ; fields = Arp.Fields.Of_always.value send_fields
      ; start = start_send.value
      ; abort = gnd
      }
  in
  Always.(
    compile
      [ if_
          is_init.value
          [ if_
              valid_request_pending
              [ when_
                  (arp_tx.ready_to_start &: ~:(cur_handled.value))
                  [ cur_handled <-- vdd
                  ; start_send <-- vdd
                  ; Addr.Mac.Of_always.assign dst_mac arp_rx.fields.sha
                  ; Arp.Fields.Of_always.assign
                      send_fields
                      { oper = of_bits Arp.Oper.reply
                      ; sha = i.fpga_mac
                      ; spa = i.fpga_ip
                      ; tha = arp_rx.fields.sha
                      ; tpa = arp_rx.fields.spa
                      }
                  ]
              ]
            @@ else_ [ cur_handled <-- gnd ]
          ]
        @@ else_
             [ when_
                 i.init
                 [ is_init <-- vdd
                 ; Addr.Mac.Of_always.assign dst_mac broadcast_mac
                 ; Arp.Fields.Of_always.assign
                     send_fields
                     { oper = of_bits Arp.Oper.request
                     ; sha = i.fpga_mac
                     ; spa = i.fpga_ip
                     ; tha = broadcast_mac
                     ; tpa = i.fpga_ip
                     }
                 ; start_send <-- vdd
                 ]
             ]
      ]);
  { tx = arp_tx.tx; dst_mac = Addr.Mac.Of_always.value dst_mac }
;;

let name = "arp_handler"

include functor Component.Make
