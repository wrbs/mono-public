open! Core
open! Hardcaml_home
open! Hardcaml_home_networking
open! Signal

let ip_equal a b = Addr.Ip.Of_signal.pack a ==: Addr.Ip.Of_signal.pack b
let mac_equal a b = Addr.Mac.Of_signal.pack a ==: Addr.Mac.Of_signal.pack b

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; header_in : 'a Ethernet.Header.t
    ; rx : 'a Packet_stream.t
    ; init : 'a
    ; fpga_mac : 'a Addr.Mac.t
    ; fpga_ip : 'a Addr.Ip.t
    ; udp_out : 'a Udp_interface.Out.t
    ; rx_valid : 'a
    ; tx_ready : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { tx : 'a Packet_stream.t
    ; header_out : 'a Ethernet.Header.t
    ; udp_in : 'a Udp_interface.In.t
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let clocking = i.clocking in
  let reg_spec = Clocking.to_spec clocking in
  let%hw is_init = reg_fb reg_spec ~width:1 ~f:(fun prev -> prev |: i.init) in
  let ip_rx =
    Ipv4.Rx.hierarchical
      scope
      { clocking; rx = i.rx; rx_valid = i.rx_valid; ethertype = i.header_in.ethertype }
  in
  let udp_rx =
    Udp.Rx.hierarchical
      scope
      { clocking
      ; rx = ip_rx.payload
      ; rx_valid = i.rx_valid
      ; ip_protocol = ip_rx.header.protocol
      ; ip_length = ip_rx.header.payload_length
      }
  in
  let%hw start_valid =
    is_init &: mac_equal i.fpga_mac i.header_in.dst &: ip_equal i.fpga_ip ip_rx.header.dst
  in
  let gated_payload, ~running:_ =
    Packet_stream.gate scope udp_rx.payload ~start_valid ~reg_spec
  in
  let udp_tx, wire_udp_tx = Udp.Tx.hierarchical' scope in
  let ip_tx =
    Ipv4.Tx.hierarchical
      scope
      { clocking
      ; tx_ready = i.tx_ready
      ; header =
          Ipv4.Mini_header.to_header_without_checksum
            { src = i.fpga_ip
            ; dst = i.udp_out.dst_ip
            ; payload_length = udp_tx.length
            ; protocol = of_bits Udp.ip_protocol
            }
      ; payload = udp_tx.tx
      }
  in
  wire_udp_tx
    { clocking
    ; tx_ready = ip_tx.payload_ready
    ; ports = { src = i.udp_out.src_port; dst = i.udp_out.dst_port }
    ; payload_length = i.udp_out.payload_length
    ; payload = i.udp_out.tx
    };
  { tx = ip_tx.tx
  ; header_out =
      { src = i.fpga_mac; dst = i.udp_out.dst_mac; ethertype = of_bits Ipv4.ethertype }
  ; udp_in =
      { src_mac = i.header_in.src
      ; src_ip = ip_rx.header.src
      ; src_port = udp_rx.ports.src
      ; dst_port = udp_rx.ports.dst
      ; payload_length = udp_rx.payload_length
      ; rx = gated_payload
      ; rx_valid = udp_rx.valid
      ; tx_ready = udp_tx.payload_ready
      }
  }
;;

let name = "ip_udp_handler"

include functor Component.Make
