open! Core
open! Hardcaml_home
open! Hardcaml_home_networking
open! Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; hw_rx : 'a Rmii.Hw.Rx.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; udp_out : 'a Udp_interface.Out.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { hw_tx : 'a Rmii.Hw.Tx.t
    ; udp_in : 'a Udp_interface.In.t
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let clocking = i.clocking in
  let rmii_rx = Rmii.Rx.hierarchical scope { clocking; hw = i.hw_rx; ignore_crc = gnd } in
  let ethernet_rx =
    Ethernet.Rx.hierarchical scope { clocking; rx = rmii_rx.rx; rx_valid = rmii_rx.valid }
  in
  let arp_handler, wire_arp_handler = Arp_handler.hierarchical' scope in
  let ip_udp_handler, wire_ip_udp_handler = Ip_udp_handler.hierarchical' scope in
  let tx_arb, wire_tx_arb = Tx_arbitrator.hierarchical' scope in
  wire_arp_handler
    { clocking
    ; init = i.init
    ; fpga_mac = i.fpga_mac
    ; fpga_ip = i.fpga_ip
    ; rx = ethernet_rx.payload
    ; rx_valid = ethernet_rx.valid
    ; ethertype = ethernet_rx.header.ethertype
    ; tx_ready = tx_arb.ready_a
    };
  wire_ip_udp_handler
    { clocking
    ; init = i.init
    ; fpga_mac = i.fpga_mac
    ; fpga_ip = i.fpga_ip
    ; rx = ethernet_rx.payload
    ; rx_valid = ethernet_rx.valid
    ; header_in = ethernet_rx.header
    ; tx_ready = tx_arb.ready_b
    ; udp_out = i.udp_out
    };
  let ethernet_tx, wire_ethernet_tx = Ethernet.Tx.hierarchical' scope in
  wire_tx_arb
    { clocking
    ; ready = ethernet_tx.payload_ready
    ; tx_a = arp_handler.tx
    ; tx_b = ip_udp_handler.tx
    };
  let rmii_tx =
    Rmii.Tx.hierarchical
      scope
      { clocking; tx = ethernet_tx.tx; last_header_byte = ethernet_tx.last_header_byte }
  in
  let header =
    Ethernet.Header.Of_signal.mux
      tx_arb.b_emitting
      [ { src = i.fpga_mac; dst = arp_handler.dst_mac; ethertype = of_bits Arp.ethertype }
      ; ip_udp_handler.header_out
      ]
  in
  wire_ethernet_tx { clocking; tx_ready = rmii_tx.ready; header; payload = tx_arb.tx };
  { hw_tx = rmii_tx.hw; udp_in = ip_udp_handler.udp_in }
;;

let name = "network_stack"

include functor Component.Make
