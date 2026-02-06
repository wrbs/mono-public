open! Core
open! Hardcaml
open! Signal
open Helpers

open struct
  open Hardcaml_hobby_boards
  module Axi32 = Ethernet.Axi32
  module Ethernet_types = Ethernet_types
end

(* Interface *)

module From_ethernet = struct
  type 'a t =
    { axi : 'a Axi32.Source.t
    ; rx_error : 'a
    ; tx_ready : 'a Axi32.Dest.t
    }
  [@@deriving hardcaml]
end

module To_ethernet = struct
  type 'a t = { axi : 'a Axi32.Source.t } [@@deriving hardcaml]
end

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; uart : 'a Byte_stream.Rx.t
    ; ethernet : 'a From_ethernet.t
    ; downstream : 'a Byte_stream.Tx.t
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { uart : 'a Byte_stream.Tx.t
    ; ethernet : 'a To_ethernet.t
    ; downstream : 'a Byte_stream.Rx.t
    }
  [@@deriving hardcaml]
end

(* protocol: 

UART: 

C<u32 fpga ip><u16 fpga port><u32 host ip><u16 host port> *)

(* Implementation *)

let ip_bits = 32
let mac_bits = 48
let port_bits = 16
let fpga_mac = Bits.of_hex ~width:mac_bits "be913e0c1bb2"

module Netconsts = struct
  let mtu = 1500
  let ip_header_len = 20
  let udp_header_len = 8
  let my_header_len = 8
  let max_data = mtu - ip_header_len - udp_header_len - my_header_len

  let ip_header_template =
    let to_fill = 0 in
    Ethernet_types.Ipv4.Of_bits.of_unsigned_ints
      { version = 4
      ; ihl = 5
      ; dscp = 0
      ; ecn = 0
      ; length = to_fill
      ; identification = 0
      ; flags = 0b010
      ; fragment_offset = 0
      ; ttl = 64
      ; protocol = 17
      ; checksum = to_fill
      ; src_ip = to_fill
      ; dst_ip = to_fill
      }
  ;;

  let init_partial_checksum () =
    let open Bits in
    ip_header_template
    |> Ethernet_types.Ipv4.Of_bits.pack
    |> split_msb ~part_width:16
    |> List.reduce_exn ~f:(ones_complement_add' (module Bits))
  ;;
end

module Sender = struct
  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; downstream : 'a Byte_stream.Tx.t
      ; flush : 'a
      ; conf : 'a Network_conf.t
      ; partial_checksum : 'a
      ; host_mac : 'a [@width mac_bits]
      ; configuration_valid : 'a
      }
  end

  module O = struct
    type 'a t =
      { packet_consumed : 'a
      ; payload_ready : 'a
      ; ether_source : 'a Axi32.Source.t
      }
  end
end

(* let create scope (i : _ I.t) : _ O.t =
  let reg_spec = Clocking.to_spec i.clocking in
  let%tydi { conf; configured; uart } =
    Configurator.create
      (Scope.sub_scope scope "configurator")
      { clocking = i.clocking; uart = i.uart }
  in
  let%hw_var host_mac = Always.Variable.reg ~width:mac_bits reg_spec in
  assert false
;; *)
