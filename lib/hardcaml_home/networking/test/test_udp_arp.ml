(* open! Core
open! Hardcaml_home
open! Hardcaml_home_networking
open! Sim_helpers

module%test Full_end_to_end = struct
  let host_mac = Addr.Mac.Const.of_string "10:00:00:00:00:01"
  let fpga_mac = Addr.Mac.Const.of_string "20:00:00:00:00:02"
  let host_ip = Addr.Ip.Const.of_string "1.2.3.4"
  let fpga_ip = Addr.Ip.Const.of_string "5.6.7.8"

  module Bench = struct
    open Signal

    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; hw_rx : 'a Rmii.Hw.Rx.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { hw_tx : 'a Rmii.Hw.Tx.t } [@@deriving hardcaml]
    end

    module Sender_state = struct
      type t =
        | Wait
        | Hi
        | Lo
        | Stop
      [@@deriving sexp_of, compare ~localize, enumerate]
    end

    let sender ~scope ~reg_spec ~length ~trigger =
      let scope = Scope.sub_scope scope "sender" in
      let%hw.Always.State_machine state =
        Always.State_machine.create (module Sender_state) reg_spec
      in
      let%hw.Packet_stream.Of_always tx = Packet_stream.Of_always.wire zero in
      Always.(compile [ state.switch [ Wait, []; Hi, []; Lo, []; Stop, [] ] ]);
      assert false
    ;;

    let create scope (i : _ I.t) : _ O.t =
      let stack, wire_stack = Hardcaml_home_udp_arp.Network_stack.hierarchical' scope in
      wire_stack
        { clocking = i.clocking
        ; hw_rx = i.hw_rx
        ; init = vdd
        ; fpga_mac = of_bits fpga_mac
        ; fpga_ip = of_bits fpga_ip
        ; udp_out =
            { dst_mac = stack.udp_in.src_mac
            ; dst_ip = stack.udp_in.src_ip
            ; src_port = stack.udp_in.dst_port
            ; dst_port = stack.udp_in.src_port
            ; payload_length = of_unsigned_int 2 ~width:16
            ; tx = assert false
            }
        };
      assert false
    ;;
  end

  include Make_sim (Hardcaml_home_udp_arp.Network_stack)

  let%expect_test "full test" =
    let () =
      testbench
      @@ fun sim ->
      let i = inputs sim in
      let driver = Ethernet_driver.create i.hw_rx in
      assert false
    in
    [%expect {|||}]
  ;;
end *)
