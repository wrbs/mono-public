open! Core
open! Hardcaml
open! Hardcaml_home_networking
open! Signal

let fpga_mac = Addr.Mac.Const.of_string "76:24:a0:4b:aa:94" |> Addr.Mac.map ~f:of_bits
let fpga_ip = Addr.Ip.Const.of_string "10.0.2.1" |> Addr.Ip.map ~f:of_bits

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; init : 'a
    ; hw_rx : 'a Rmii.Hw.Rx.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { hw_tx : 'a Rmii.Hw.Tx.t
    ; start : 'a
    ; stop : 'a
    ; abort : 'a
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let clocking = i.clocking in
  let rmii_rx = Rmii.Rx.hierarchical scope { clocking; hw = i.hw_rx; ignore_crc = gnd } in
  let ethernet_rx =
    Ethernet.Rx.hierarchical scope { clocking; rx = rmii_rx.rx; rx_valid = rmii_rx.valid }
  in
  let arp_ready = wire 1 in
  let arp_handler =
    Arp_handler.hierarchical
      scope
      { clocking
      ; init = i.init
      ; fpga_mac
      ; fpga_ip
      ; rx = ethernet_rx.payload
      ; rx_valid = ethernet_rx.valid
      ; ethertype = ethernet_rx.header.ethertype
      ; tx_ready = arp_ready
      }
  in
  let ethernet_ready = wire 1 in
  let ethernet_tx =
    Ethernet.Tx.hierarchical
      scope
      { clocking
      ; tx_ready = ethernet_ready
      ; header =
          { src = fpga_mac; dst = arp_handler.dst_mac; ethertype = of_bits Arp.ethertype }
      ; payload = arp_handler.tx
      }
  in
  assign arp_ready ethernet_tx.payload_ready;
  let rmii_tx =
    Rmii.Tx.hierarchical
      scope
      { clocking; tx = ethernet_tx.tx; last_header_byte = ethernet_tx.last_header_byte }
  in
  assign ethernet_ready rmii_tx.ready;
  { hw_tx = rmii_tx.hw
  ; start = rmii_rx.rx.start
  ; stop = rmii_rx.rx.stop
  ; abort = rmii_rx.drop
  }
;;
