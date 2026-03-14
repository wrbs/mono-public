(* Lowest level interface *)

open! Core
open! Hardcaml
open! Hardcaml_home
open! Hardcaml_home_networking
open! Sim_helpers

module%test Rmii_tx_ll = struct
  include Make_sim (Rmii.Tx_ll)

  let consume_preamble ?early_break sim =
    let o = outputs sim ~edge:Before in
    let o_a = outputs sim ~edge:After in
    let preamble_cycles = (7 * 4) + 3 in
    let rec aux n =
      [%test_result: Bits.t] !(o.hw.txen) ~expect:Bits.vdd;
      match n < preamble_cycles with
      | true ->
        [%test_result: Bits.t] !(o.hw.txd) ~expect:(Bits.of_string "01");
        let stop_early =
          match early_break with
          | None -> false
          | Some cyc -> n >= cyc
        in
        (match stop_early with
         | true -> ()
         | false ->
           Cyclesim.cycle sim;
           aux (n + 1))
      | false ->
        [%test_result: Bits.t] !(o.hw.txd) ~expect:(Bits.of_string "11");
        [%test_result: Bits.t] !(o_a.sfd_sent) ~expect:Bits.vdd;
        [%test_result: Bits.t] !(o_a.ready_for_data) ~expect:Bits.vdd
    in
    aux 0
  ;;

  let init sim =
    let i, o = io sim ~edge:Before in
    Cyclesim.cycle sim;
    [%test_result: Bits.t] !(o.ready_to_start) ~expect:Bits.vdd;
    (* start *)
    i.start := Bits.vdd;
    Cyclesim.cycle sim;
    consume_preamble sim
  ;;

  let wait_ipg sim =
    let ipg_cycles = 96 / 2 in
    let o_before = outputs sim ~edge:Before in
    let o_after = outputs sim ~edge:After in
    let rec aux n =
      Cyclesim.cycle sim;
      let num_cycles = n + 1 in
      [%test_result: Bits.t] !(o_before.hw.txen) ~expect:Bits.gnd;
      match num_cycles < ipg_cycles with
      | true ->
        [%test_result: Bits.t] !(o_after.ready_to_start) ~expect:Bits.gnd;
        aux num_cycles
      | false ->
        [%test_result: Bits.t] !(o_after.ready_to_start) ~expect:Bits.vdd;
        ()
    in
    aux 0
  ;;

  let collect_output_le sim ~cycles =
    let o = outputs sim ~edge:Before in
    let out = ref Bits.empty in
    let rec aux ~left =
      Cyclesim.cycle sim;
      let cycles_left = left - 1 in
      [%test_result: Bits.t] !(o.hw.txen) ~expect:Bits.vdd;
      let new_out =
        let next = !(o.hw.txd) in
        if Bits.is_empty !out then next else Bits.(next @: !out)
      in
      out := new_out;
      match cycles_left with
      | 0 -> !out
      | _ -> aux ~left:cycles_left
    in
    aux ~left:cycles
  ;;

  let collect_byte sim = collect_output_le sim ~cycles:4

  let%expect_test "Start then abort" =
    let () =
      testbench
      @@ fun sim ->
      let i = inputs sim in
      init sim;
      (* abort without sfd *)
      i.stop_abort := Bits.vdd;
      wait_ipg sim;
      ()
    in
    [%expect {| |}]
  ;;

  let%expect_test "Abort early" =
    let () =
      testbench
      @@ fun sim ->
      let i, o = io sim ~edge:Before in
      Cyclesim.cycle sim;
      [%test_result: Bits.t] !(o.ready_to_start) ~expect:Bits.vdd;
      (* start *)
      i.start := Bits.vdd;
      Cyclesim.cycle sim;
      consume_preamble sim ~early_break:10;
      (* abort without sfd *)
      i.stop_abort := Bits.vdd;
      wait_ipg sim;
      ()
    in
    [%expect {| |}]
  ;;

  let%expect_test "Emit data then stop with crc" =
    let data = [ 4; 5; 6; 7 ] |> List.map ~f:(Bits.of_unsigned_int ~width:8) in
    let () =
      testbench
      @@ fun sim ->
      let i, o = io sim ~edge:After in
      init sim;
      List.iter data ~f:(fun v ->
        [%test_result: Bits.t] !(o.ready_for_data) ~expect:Bits.vdd;
        i.data := v;
        let byte = collect_byte sim in
        [%test_result: Bits.t] byte ~expect:v);
      ();
      i.stop_with_crc := Bits.vdd;
      let rest = List.init 4 ~f:(fun _ -> collect_byte sim) in
      let crc_le = Bits.concat_lsb rest in
      let expected_crc_le =
        Bits.concat_msb data |> Crc.add_crc32_le |> Bits.sel_bottom ~width:32
      in
      printf "%08X" (Bits.to_unsigned_int expected_crc_le);
      [%expect {| 85B8D360 |}];
      printf "%08X" (Bits.to_unsigned_int crc_le);
      [%expect {| 85B8D360 |}]
    in
    [%expect {| |}]
  ;;

  let%expect_test "Emit data then stop with invalid" =
    let data = [ 4; 5; 6; 7 ] |> List.map ~f:(Bits.of_unsigned_int ~width:8) in
    let () =
      testbench
      @@ fun sim ->
      let i, o = io sim ~edge:After in
      init sim;
      List.iter data ~f:(fun v ->
        [%test_result: Bits.t] !(o.ready_for_data) ~expect:Bits.vdd;
        i.data := v;
        let byte = collect_byte sim in
        [%test_result: Bits.t] byte ~expect:v);
      ();
      i.stop_with_crc := Bits.vdd;
      i.stop_abort := Bits.vdd;
      let rest = List.init 4 ~f:(fun _ -> collect_byte sim) in
      let crc = Bits.concat_msb rest in
      printf "%08X" (Bits.to_unsigned_int crc);
      [%expect {| BADCBADA |}]
    in
    [%expect {| |}]
  ;;
end

module%test Full_udp_tx = struct
  module Combined = struct
    open Signal

    module Payload_sender = struct
      module I = struct
        type 'a t =
          { clocking : 'a Clocking.t
          ; tx_ready : 'a
          ; payload_length : 'a [@bits 16]
          }
        [@@deriving hardcaml]
      end

      module O = struct
        type 'a t = { tx : 'a Packet_stream.t } [@@deriving hardcaml]
      end

      module State = struct
        type t =
          | Start
          | Data
          | Done
        [@@deriving sexp_of, compare ~localize, enumerate]
      end

      let create scope (i : _ I.t) : _ O.t =
        let reg_spec = Clocking.to_spec i.clocking in
        let%hw_var count = Always.Variable.reg ~width:16 reg_spec in
        let%hw.Always.State_machine state =
          Always.State_machine.create (module State) reg_spec
        in
        let%hw_var stop = Always.Variable.wire ~default:gnd () in
        Always.(
          compile
            [ state.switch
                [ Start, [ state.set_next Data ]
                ; ( Data
                  , [ when_
                        i.tx_ready
                        [ if_
                            (count.value ==: i.payload_length)
                            [ stop <-- vdd; state.set_next Done ]
                          @@ else_ [ incr count ]
                        ]
                    ] )
                ; Done, []
                ]
            ]);
        { tx =
            { data = sel_bottom count.value ~width:8
            ; start = state.is Start
            ; stop = stop.value
            ; abort = gnd
            }
        }
      ;;

      let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
        let module H = Hierarchy.In_scope (I) (O) in
        H.hierarchical ~scope ~name:"payload_sender" create input
      ;;
    end

    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; src_mac : 'a Addr.Mac.t
        ; dst_mac : 'a Addr.Mac.t
        ; src_ip : 'a Addr.Ip.t
        ; dst_ip : 'a Addr.Ip.t
        ; src_port : 'a [@bits 16]
        ; dst_port : 'a [@bits 16]
        ; payload_length : 'a [@bits 16]
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { hw : 'a Rmii.Hw.Tx.t } [@@deriving hardcaml]
    end

    module Ready = struct
      type 'a t =
        { udp : 'a
        ; ip : 'a
        ; ethernet : 'a
        ; rmii : 'a
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let clocking = i.clocking in
      let%hw.Ready.Of_signal ready = Ready.Of_signal.wires () in
      let payload =
        Payload_sender.hierarchical
          scope
          { clocking; tx_ready = ready.udp; payload_length = i.payload_length }
      in
      let udp =
        Udp.Tx.hierarchical
          scope
          { clocking
          ; tx_ready = ready.ip
          ; ports = { src = i.src_port; dst = i.dst_port }
          ; payload_length = i.payload_length
          ; payload = payload.tx
          }
      in
      assign ready.udp udp.payload_ready;
      let ip =
        Ipv4.Tx.hierarchical
          scope
          { clocking
          ; tx_ready = ready.ethernet
          ; header =
              Ipv4.Mini_header.to_header_without_checksum
                { payload_length = udp.length
                ; protocol = of_bits Udp.ip_protocol
                ; src = i.src_ip
                ; dst = i.dst_ip
                }
          ; payload = udp.tx
          }
      in
      assign ready.ip ip.payload_ready;
      let ethernet =
        Ethernet.Tx.hierarchical
          scope
          { clocking
          ; tx_ready = ready.rmii
          ; header =
              { src = i.src_mac; dst = i.dst_mac; ethertype = of_bits Ipv4.ethertype }
          ; payload = ip.tx
          }
      in
      assign ready.ethernet ethernet.payload_ready;
      let rmii =
        Rmii.Tx.hierarchical
          scope
          { clocking; tx = ethernet.tx; last_header_byte = ethernet.last_header_byte }
      in
      assign ready.rmii rmii.ready;
      { hw = rmii.hw }
    ;;
  end

  include Make_sim (Combined)

  let%expect_test "Packet dump" =
    let () =
      testbench
      @@ fun sim ->
      let i, o = io sim ~edge:Before in
      Addr.Mac.iter2 i.src_mac (Addr.Mac.Const.of_string "01:23:45:67:89:ab") ~f:( := );
      Addr.Mac.iter2 i.dst_mac (Addr.Mac.Const.of_string "fa:fa:fa:fa:01:23") ~f:( := );
      Addr.Ip.iter2 i.src_ip (Addr.Ip.Const.of_string "1.2.3.4") ~f:( := );
      Addr.Ip.iter2 i.dst_ip (Addr.Ip.Const.of_string "5.6.7.8") ~f:( := );
      i.src_port := Bits.of_unsigned_int ~width:16 118;
      i.dst_port := Bits.of_unsigned_int ~width:16 247;
      i.payload_length := Bits.of_unsigned_int ~width:16 56;
      let out = Vec.create () in
      Cyclesim.with_timeout sim ~timeout:1000 ~f:(fun sim ->
        let rec loop () =
          Cyclesim.cycle sim;
          match Bits.to_bool !(o.hw.txen) with
          | false -> ()
          | true ->
            Vec.push_back out !(o.hw.txd);
            loop ()
        in
        loop ());
      let s =
        Vec.to_list out
        |> List.chunks_of ~length:4
        |> List.map ~f:(fun parts -> Bits.concat_lsb parts |> Bits.to_char)
        |> String.of_char_list
      in
      print_s [%sexp (s : String.Hexdump.t)];
      [%expect
        {|
        ("00000000  55 55 55 55 55 55 55 d5  fa fa fa fa 01 23 01 23  |UUUUUUU......#.#|"
         "00000010  45 67 89 ab 08 00 45 00  00 54 00 00 40 00 40 11  |Eg....E..T..@.@.|"
         "00000020  2a 86 01 02 03 04 05 06  07 08 00 76 00 f7 00 40  |*..........v...@|"
         "00000030  00 00 00 01 02 03 04 05  06 07 08 09 0a 0b 0c 0d  |................|"
         "00000040  0e 0f 10 11 12 13 14 15  16 17 18 19 1a 1b 1c 1d  |................|"
         "00000050  1e 1f 20 21 22 23 24 25  26 27 28 29 2a 2b 2c 2d  |.. !\"#$%&'()*+,-|"
         "00000060  2e 2f 30 31 32 33 34 35  36 37 b3 0e e8 5e        |./01234567...^|")
        |}]
    in
    [%expect {| |}]
  ;;
end

module%test Full_arp_tx = struct
  module Combined = struct
    open Signal

    module I = struct
      type 'a t =
        { clocking : 'a Clocking.t
        ; fields : 'a Arp.Fields.t
        }
      [@@deriving hardcaml]
    end

    module O = struct
      type 'a t = { hw : 'a Rmii.Hw.Tx.t } [@@deriving hardcaml]
    end

    module Ready = struct
      type 'a t =
        { ethernet : 'a
        ; rmii : 'a
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let clocking = i.clocking in
      let reg_spec = Clocking.to_spec clocking in
      let%hw.Ready.Of_signal ready = Ready.Of_signal.wires () in
      let start_n = reg reg_spec vdd in
      let arp =
        Arp.Tx.hierarchical
          scope
          { clocking
          ; tx_ready = ready.ethernet
          ; fields = i.fields
          ; start = ~:start_n
          ; abort = gnd
          }
      in
      let ethernet =
        Ethernet.Tx.hierarchical
          scope
          { clocking
          ; tx_ready = ready.rmii
          ; header =
              { src = i.fields.sha
              ; dst = i.fields.tha
              ; ethertype = of_bits Arp.ethertype
              }
          ; payload = arp.tx
          }
      in
      assign ready.ethernet ethernet.payload_ready;
      let rmii =
        Rmii.Tx.hierarchical
          scope
          { clocking; tx = ethernet.tx; last_header_byte = ethernet.last_header_byte }
      in
      assign ready.rmii rmii.ready;
      { hw = rmii.hw }
    ;;
  end

  include Make_sim (Combined)

  let%expect_test "Packet dump" =
    let () =
      testbench ~vcd:"~/waves/arp_tx.vcd"
      @@ fun sim ->
      let i, o = io sim ~edge:Before in
      i.fields.oper := Arp.Oper.request;
      Addr.Mac.iter2 i.fields.sha (Addr.Mac.Const.of_string "01:23:45:67:89:ab") ~f:( := );
      Addr.Ip.iter2 i.fields.spa (Addr.Ip.Const.of_string "1.2.3.4") ~f:( := );
      Addr.Mac.iter2 i.fields.tha (Addr.Mac.Const.of_string "fa:fa:fa:fa:01:23") ~f:( := );
      Addr.Ip.iter2 i.fields.tpa (Addr.Ip.Const.of_string "5.6.7.8") ~f:( := );
      let out = Vec.create () in
      Cyclesim.with_timeout sim ~timeout:1000 ~f:(fun sim ->
        let rec loop () =
          Cyclesim.cycle sim;
          match Bits.to_bool !(o.hw.txen) with
          | false -> ()
          | true ->
            Vec.push_back out !(o.hw.txd);
            loop ()
        in
        loop ());
      let s =
        Vec.to_list out
        |> List.chunks_of ~length:4
        |> List.map ~f:(fun parts -> Bits.concat_lsb parts |> Bits.to_char)
        |> String.of_char_list
      in
      print_s [%sexp (s : String.Hexdump.t)];
      [%expect {|
        ("00000000  55 55 55 55 55 55 55 d5  fa fa fa fa 01 23 01 23  |UUUUUUU......#.#|"
         "00000010  45 67 89 ab 08 06 00 01  08 00 06 04 00 01 01 23  |Eg.............#|"
         "00000020  45 67 89 ab 01 02 03 04  fa fa fa fa 01 23 05 06  |Eg...........#..|"
         "00000030  07 08 00 00 00 00 00 00  00 00 00 00 00 00 00 00  |................|"
         "00000040  00 00 00 00 dc 7a 76 09                           |.....zv.|")
        |}]
    in
    [%expect
      {| Saved waves to ~/waves/arp_tx.vcd |}]
  ;;
end
