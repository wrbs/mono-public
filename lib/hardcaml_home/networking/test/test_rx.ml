open! Core
open! Hardcaml
open! Hardcaml_home
open! Hardcaml_home_networking
open! Sim_helpers

let test_tcp_ack =
  "ba7ecefe4fdb36b616dc1fa108004500002848e400004006b8c60a725ab30a000a01ef7700169c6f47e622740a79501020d515090000723aa262"
;;

let test_udp_no_crc =
  "01005e4c4e4b36b616dc1fa108004500002dcb63000001115aa00a725ab3e04c4e4b51ad51ad0019fc2261626c73645f76010364782724663e5946"
;;

let test_arp_no_crc =
  "010203040506102030405060080600010800060400021020304050600a0000010102030405060a725ab300000000000000000000"
;;

let test_udp () = Crc.add_crc32_le (hex_bits test_udp_no_crc)
let test_arp () = Crc.add_crc32_le (hex_bits test_arp_no_crc)

let show_ethernet_header' (header : Bits.t Ethernet.Header.t) =
  let src = header.src in
  let dst = header.dst in
  let ethertype = sprintf "%04X" (Bits.to_unsigned_int header.ethertype) in
  print_s
    [%message (dst : Addr.Mac.Const.t) (src : Addr.Mac.Const.t) (ethertype : string)]
;;

let show_ethernet_header x = show_ethernet_header' (Ethernet.Header.map x ~f:( ! ))

let show_ip_mini_header' (header : Bits.t Ipv4.Mini_header.t) =
  let%tydi { payload_length; protocol; src; dst } = header in
  let payload_length = Bits.to_unsigned_int payload_length in
  let protocol = Bits.to_unsigned_int protocol in
  print_s
    [%message
      (payload_length : int)
        (protocol : int)
        (src : Addr.Ip.Const.t)
        (dst : Addr.Ip.Const.t)]
;;

let show_ip_mini_header x = show_ip_mini_header' (Ipv4.Mini_header.map x ~f:( ! ))

let drive_input ?f (sim : _ Cyclesim.t) (i : Bits.t ref Rmii.Rx.I.t) ~bits =
  i.hw.crsdv := Bits.vdd;
  i.hw.rxerr := Bits.gnd;
  Bits.split_msb bits ~exact:false ~part_width:8
  |> List.concat_map ~f:(Bits.split_lsb ~exact:true ~part_width:2)
  |> List.iter ~f:(fun data ->
    i.hw.rxd := data;
    Cyclesim.cycle sim;
    Option.iter f ~f:(fun f -> f ()))
;;

let drive_frame ?f (sim : _ Cyclesim.t) (i : Bits.t ref Rmii.Rx.I.t) ~bits =
  Cyclesim.cycle sim;
  drive_input ?f sim i ~bits:(with_sfd bits);
  i.hw.crsdv := Bits.gnd;
  Cyclesim.cycle_before_clock_edge sim;
  Option.iter f ~f:(fun f -> f ())
;;

(* Cyclesim.cycle sim;
  Option.iter f ~f:(fun f -> f ()) *)

module%test Rmii_rx = struct
  include Make_sim (Rmii.Rx)

  let show_state sim () =
    let i, o = io sim ~edge:Before in
    if Bits.to_bool !(i.hw.crsdv)
    then (
      let v = Bits.Binary.to_string !(i.hw.rxd) in
      printf "%s" (String.suffix v 2));
    if Bits.to_bool !(o.rx.abort)
    then print_endline "\nABORT"
    else if Bits.to_bool !(o.rx.start)
    then print_endline "\nstart"
    else (
      if Bits.to_bool !(o.valid)
      then printf "\ndata=%02X\n" (Bits.to_unsigned_int !(o.rx.data));
      if Bits.to_bool !(o.rx.stop) then print_endline "stop")
  ;;

  let%expect_test "Stopping mid-way through" =
    let waves, sim = create_waveform () in
    drive_frame sim ~f:(show_state sim) (inputs sim) ~bits:(hex_bits "123");
    [%expect
      {|
      010101010101011110
      start
      00010011
      data=12
      00
      ABORT
      |}];
    if false then Hardcaml_waveterm.Waveform.expect waves ~display_width:1000;
    [%expect {| |}]
  ;;

  let%expect_test "Happy path (ignoring crc) " =
    let waves, sim = create_waveform () in
    let i = inputs sim in
    i.ignore_crc := Bits.vdd;
    drive_frame sim ~f:(show_state sim) (inputs sim) ~bits:(hex_bits "12345678");
    [%expect
      {|
      010101010101011110
      start
      00010000
      data=12
      01110010
      data=34
      01010100
      data=56
      101101
      data=78
      |}];
    if false then Hardcaml_waveterm.Waveform.expect waves ~display_width:1000;
    [%expect {| |}]
  ;;

  let%expect_test "real packet (not skipping crc)" =
    let sim = create () in
    drive_frame sim (inputs sim) ~bits:(hex_bits test_tcp_ack);
    let i, o = io sim ~edge:Before in
    i.hw.crsdv := Bits.gnd;
    Cyclesim.cycle sim;
    print_s [%sexp (o : Bits.Hex.t ref O.t)];
    [%expect
      {|
      ((rx ((data 8'h62) (start 1'h0) (stop 1'h0) (abort 1'h0))) (valid 1'h1)
       (drop 1'h0))
      |}]
  ;;
end

module%test Rx = struct
  module Combined = struct
    module I = Rmii.Rx.I
    module O = Ethernet.Rx.O

    let create scope (i : _ I.t) : _ O.t =
      let%tydi { rx; valid = rx_valid; drop = _ } = Rmii.Rx.hierarchical scope i in
      Ethernet.Rx.hierarchical scope { clocking = i.clocking; rx; rx_valid }
    ;;
  end

  include Make_sim (Combined)

  let send (sim : Sim.t) (bits : Bits.t) = drive_input sim (inputs sim) ~bits

  let print_header sim =
    let o = outputs sim ~edge:After in
    show_ethernet_header o.header
  ;;

  let%expect_test "Check header parsing" =
    let sim = create () in
    send sim sfd;
    send sim (hex_bits "0123");
    print_header sim;
    [%expect {| ((dst 00:00:00:00:00:00) (src 00:00:00:00:00:00) (ethertype 0001)) |}];
    send sim (hex_bits "456789ABFFEEDDCCBBAADEADFFFFFF");
    print_header sim;
    [%expect {| ((dst 01:23:45:67:89:ab) (src ff:ee:dd:cc:bb:aa) (ethertype DEAD)) |}]
  ;;

  let%expect_test "crc calc" =
    let open Bits in
    let packet = hex_bits test_tcp_ack |> drop_bottom ~width:32 in
    let out =
      Bits.split_msb packet ~part_width:8
      |> List.fold ~init:(ones 32) ~f:(fun crc bits ->
        Crc.update_bits bits ~crc ~polynomial:Crc.crc32)
      |> Bits.( ~: )
      |> Bits.split_msb ~part_width:8
      |> Bits.concat_lsb
    in
    print_endline (String.drop_prefix (Bits.Hex.to_string out) 4);
    (* 723aa262 *)
    [%expect {| 723aa262 |}]
  ;;

  let%expect_test "Check full pipeline" =
    let () =
      testbench
      @@ fun sim ->
      let driver = Ethernet_driver.create (inputs sim).hw in
      Ethernet_driver.add_packet driver ~packet:(with_sfd (test_udp ()));
      let update_inputs _ = Ethernet_driver.update_inputs driver in
      let o = outputs sim ~edge:Before in
      let stream_before_cycle = o.payload in
      skip_to_stream_start sim ~stream_before_cycle ~update_inputs;
      print_header sim;
      [%expect {| ((dst 01:00:5e:4c:4e:4b) (src 36:b6:16:dc:1f:a1) (ethertype 0800)) |}];
      let valid_before_cycle = o.valid in
      let data =
        consume_stream sim ~stream_before_cycle ~valid_before_cycle ~update_inputs
      in
      print_s [%sexp (data : String.Hexdump.t)];
      [%expect
        {|
        ("00000000  45 00 00 2d cb 63 00 00  01 11 5a a0 0a 72 5a b3  |E..-.c....Z..rZ.|"
         "00000010  e0 4c 4e 4b 51 ad 51 ad  00 19 fc 22 61 62 6c 73  |.LNKQ.Q....\"abls|"
         "00000020  64 5f 76 01 03 64 78 27  24 66 3e 59 46           |d_v..dx'$f>YF|")
        |}]
    in
    [%expect {| |}]
  ;;

  let%expect_test "Check full pipeline, but bad crc" =
    let () =
      testbench
      @@ fun sim ->
      let bad_packet =
        Bits.(
          let good = test_udp () in
          msbs good @: ~:(lsb good))
      in
      let driver = Ethernet_driver.create (inputs sim).hw in
      Ethernet_driver.add_packet driver ~packet:(with_sfd bad_packet);
      let update_inputs _ = Ethernet_driver.update_inputs driver in
      let o = outputs sim ~edge:Before in
      let stream_before_cycle = o.payload in
      skip_to_stream_start sim ~stream_before_cycle ~update_inputs;
      print_header sim;
      [%expect {| ((dst 01:00:5e:4c:4e:4b) (src 36:b6:16:dc:1f:a1) (ethertype 0800)) |}];
      let valid_before_cycle = o.valid in
      let data, result =
        consume_stream_result sim ~stream_before_cycle ~valid_before_cycle ~update_inputs
      in
      print_s [%message (result : [ `Stop | `Abort ]) (data : String.Hexdump.t)];
      [%expect
        {|
        ((result Abort)
         (data
          ("00000000  45 00 00 2d cb 63 00 00  01 11 5a a0 0a 72 5a b3  |E..-.c....Z..rZ.|"
           "00000010  e0 4c 4e 4b 51 ad 51 ad  00 19 fc 22 61 62 6c 73  |.LNKQ.Q....\"abls|"
           "00000020  64 5f 76 01 03 64 78 27  24 66 3e 59              |d_v..dx'$f>Y|")))
        |}]
    in
    [%expect {| |}]
  ;;
end

module%test Ipv4_full = struct
  module Combined = struct
    module I = Rmii.Rx.I

    module O = struct
      type 'a t =
        { ethernet : 'a Ethernet.Header.t
        ; ipv4 : 'a Ipv4.Mini_header.t
        ; udp_ports : 'a Udp.Ports.t
        ; payload_length : 'a [@bits 16]
        ; payload : 'a Packet_stream.t
        ; valid : 'a
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let%tydi { rx; valid = rx_valid; drop = _ } = Rmii.Rx.hierarchical scope i in
      let%tydi { header = ethernet; payload = eth_payload; valid = eth_valid } =
        Ethernet.Rx.hierarchical scope { clocking = i.clocking; rx; rx_valid }
      in
      let%tydi { header = ipv4; payload = ip_payload; valid = ip_valid } =
        Ipv4.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; rx = eth_payload
          ; rx_valid = eth_valid
          ; ethertype = ethernet.ethertype
          }
      in
      let%tydi { ports = udp_ports; payload_length; payload; valid } =
        Udp.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; rx = ip_payload
          ; rx_valid = ip_valid
          ; ip_protocol = ipv4.protocol
          ; ip_length = ipv4.payload_length
          }
      in
      { ethernet; ipv4; udp_ports; payload_length; payload; valid }
    ;;
  end

  include Make_sim (Combined)

  let%expect_test "Check full pipeline" =
    let () =
      testbench
      @@ fun sim ->
      let driver = Ethernet_driver.create (inputs sim).hw in
      Ethernet_driver.add_packet driver ~packet:(with_sfd (test_udp ()));
      let update_inputs _ = Ethernet_driver.update_inputs driver in
      let o = outputs sim ~edge:Before in
      let stream_before_cycle = o.payload in
      skip_to_stream_start sim ~stream_before_cycle ~update_inputs;
      show_ethernet_header o.ethernet;
      show_ip_mini_header o.ipv4;
      print_s
        [%sexp
          (o.udp_ports |> Udp.Ports.map ~f:(fun x -> Bits.to_unsigned_int !x)
           : int Udp.Ports.t)];
      [%expect
        {|
        ((dst 01:00:5e:4c:4e:4b) (src 36:b6:16:dc:1f:a1) (ethertype 0800))
        ((payload_length 25) (protocol 17) (src 10.114.90.179) (dst 224.76.78.75))
        ((src 20909) (dst 20909))
        |}];
      let valid_before_cycle = o.valid in
      let data =
        consume_stream sim ~stream_before_cycle ~valid_before_cycle ~update_inputs
      in
      print_s [%sexp (data : String.Hexdump.t)];
      [%expect
        {|
        ("00000000  61 62 6c 73 64 5f 76 01  03 64 78 27 24 66 3e 59  |ablsd_v..dx'$f>Y|"
         "00000010  46                                                |F|")
        |}]
    in
    [%expect {| |}]
  ;;
end

module%test Tx_rx_roundtrip = struct
  module Combined = struct
    open Signal

    module I = struct
      type 'a t = { clocking : 'a Clocking.t } [@@deriving hardcaml]
    end

    module O = struct
      type 'a t =
        { ethernet : 'a Ethernet.Header.t
        ; ipv4 : 'a Ipv4.Mini_header.t
        ; udp_ports : 'a Udp.Ports.t
        ; payload_length : 'a [@bits 16]
        ; payload : 'a Packet_stream.t
        ; valid : 'a
        }
      [@@deriving hardcaml]
    end

    let emitter scope (i : _ I.t) : 'a Rmii.Hw.Tx.t =
      let packet = of_bits (hex_bits test_udp_no_crc) in
      let bytes = split_msb packet ~part_width:8 in
      let max_count = List.length bytes + 1 in
      let count_bits = num_bits_to_represent max_count in
      let reg_spec = Clocking.to_spec i.clocking in
      let%hw_var count = Always.Variable.reg ~width:count_bits reg_spec in
      let%hw_var start = Always.Variable.wire ~default:gnd () in
      let%hw data = mux count.value (zero 8 :: (bytes @ [ zero 8 ])) in
      let%hw stop_with_crc = count.value ==:. max_count in
      let%tydi { hw; ready_to_start; ready_for_data; sfd_sent = _ } =
        Rmii.Tx_ll.hierarchical
          scope
          { clocking = i.clocking
          ; data
          ; start = start.value
          ; stop_abort = gnd
          ; stop_with_crc
          }
      in
      Always.(
        compile
          [ if_
              (count.value ==:. 0)
              [ when_ ready_to_start [ start <-- vdd; incr count ] ]
            @@ else_
                 [ when_
                     (count.value <:. max_count)
                     [ when_ ready_for_data [ incr count ] ]
                 ]
          ]);
      hw
    ;;

    let create scope (i : _ I.t) : _ O.t =
      let%tydi tx = emitter (Scope.sub_scope scope "emitter") i in
      let%tydi { rx; valid = rx_valid; drop = _ } =
        Rmii.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; hw = { crsdv = tx.txen; rxerr = gnd; rxd = tx.txd }
          ; ignore_crc = gnd
          }
      in
      let%tydi { header = ethernet; payload = eth_payload; valid = eth_valid } =
        Ethernet.Rx.hierarchical scope { clocking = i.clocking; rx; rx_valid }
      in
      let%tydi { header = ipv4; payload = ip_payload; valid = ip_valid } =
        Ipv4.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; rx = eth_payload
          ; rx_valid = eth_valid
          ; ethertype = ethernet.ethertype
          }
      in
      let%tydi { ports = udp_ports; payload_length; payload; valid } =
        Udp.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; rx = ip_payload
          ; rx_valid = ip_valid
          ; ip_protocol = ipv4.protocol
          ; ip_length = ipv4.payload_length
          }
      in
      { ethernet; ipv4; udp_ports; payload_length; payload; valid }
    ;;
  end

  include Make_sim (Combined)

  let%expect_test "Check full pipeline, but bad crc" =
    let () =
      testbench
      @@ fun sim ->
      let update_inputs _ = () in
      let o = outputs sim ~edge:Before in
      let stream_before_cycle = o.payload in
      skip_to_stream_start sim ~stream_before_cycle ~update_inputs;
      show_ethernet_header o.ethernet;
      show_ip_mini_header o.ipv4;
      print_s
        [%sexp
          (o.udp_ports |> Udp.Ports.map ~f:(fun x -> Bits.to_unsigned_int !x)
           : int Udp.Ports.t)];
      [%expect
        {|
        ((dst 01:00:5e:4c:4e:4b) (src 36:b6:16:dc:1f:a1) (ethertype 0800))
        ((payload_length 25) (protocol 17) (src 10.114.90.179) (dst 224.76.78.75))
        ((src 20909) (dst 20909))
        |}];
      let valid_before_cycle = o.valid in
      let data =
        consume_stream sim ~stream_before_cycle ~valid_before_cycle ~update_inputs
      in
      print_s [%sexp (data : String.Hexdump.t)];
      [%expect
        {|
        ("00000000  61 62 6c 73 64 5f 76 01  03 64 78 27 24 66 3e 59  |ablsd_v..dx'$f>Y|"
         "00000010  46                                                |F|")
        |}]
    in
    [%expect {| |}]
  ;;
end

module%test Arp = struct
  module Combined = struct
    module I = Rmii.Rx.I

    module O = struct
      type 'a t =
        { ethernet : 'a Ethernet.Header.t
        ; fields : 'a Arp.Fields.t
        ; valid : 'a
        }
      [@@deriving hardcaml]
    end

    let create scope (i : _ I.t) : _ O.t =
      let%tydi { rx; valid = rx_valid; drop = _ } = Rmii.Rx.hierarchical scope i in
      let%tydi { header = ethernet; payload = eth_payload; valid = eth_valid } =
        Ethernet.Rx.hierarchical scope { clocking = i.clocking; rx; rx_valid }
      in
      let%tydi { fields; valid } =
        Arp.Rx.hierarchical
          scope
          { clocking = i.clocking
          ; rx = eth_payload
          ; rx_valid = eth_valid
          ; ethertype = ethernet.ethertype
          }
      in
      { ethernet; fields; valid }
    ;;
  end

  include Make_sim (Combined)

  let show_arp_fields' (fields : Bits.t Arp.Fields.t) =
    let%tydi { oper; sha; spa; tha; tpa } = fields in
    let oper = Bits.to_unsigned_int oper in
    print_s
      [%message
        (oper : int)
          (sha : Addr.Mac.Const.t)
          (spa : Addr.Ip.Const.t)
          (tha : Addr.Mac.Const.t)
          (tpa : Addr.Ip.Const.t)]
  ;;

  let show_arp_fields x = show_arp_fields' (Arp.Fields.map x ~f:( ! ))

  let%expect_test "Check full pipeline" =
    let () =
      testbench
      @@ fun sim ->
      let driver = Ethernet_driver.create (inputs sim).hw in
      Ethernet_driver.add_packet driver ~packet:(with_sfd (test_arp ()));
      let o = outputs sim ~edge:Before in
      Cyclesim.with_timeout sim ~timeout:1000 ~f:(fun sim ->
        while not (Bits.to_bool !(o.valid)) do
          Ethernet_driver.update_inputs driver;
          Cyclesim.cycle sim
        done);
      show_ethernet_header o.ethernet;
      show_arp_fields o.fields;
      [%expect
        {|
        ((dst 01:02:03:04:05:06) (src 10:20:30:40:50:60) (ethertype 0806))
        ((oper 2) (sha 10:20:30:40:50:60) (spa 10.0.0.1) (tha 01:02:03:04:05:06)
         (tpa 10.114.90.179))
        |}]
    in
    [%expect {| |}]
  ;;
end
