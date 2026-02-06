open! Core
open! Hardcaml
open! Signal

let buf_capacity = 1500
let size_bits = num_bits_to_represent buf_capacity

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; downstream_tx : 'a Byte_stream.Tx.t
    ; upstream_ready : 'a
    ; flush : 'a
    ; consume_packet : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { packet_ready : 'a
    ; length : 'a [@bits 16] (* of packet if packet_ready *)
    ; downstream_ready : 'a
    ; upstream_tx : 'a Byte_stream.Tx.t
    }
  [@@deriving hardcaml]
end

let create scope (i : _ I.t) : _ O.t =
  let reg_spec = Clocking.to_spec i.clocking in
  let%hw_var queued_length = Always.Variable.reg reg_spec ~width:size_bits in
  let%hw_var next_queued_length = Always.Variable.wire () ~default:queued_length.value in
  let%hw_var ready_or_released_packet_length =
    Always.Variable.cut_through_reg reg_spec ~width:size_bits
  in
  let%hw_var read_bits_left = Always.Variable.reg reg_spec ~width:size_bits in
  let%hw_var packet_queued = Always.Variable.reg reg_spec ~width:1 in
  let%hw_var rb_write_valid = Always.Variable.wire () ~default:gnd in
  (* Read state *)
  let%hw done_reading = read_bits_left.value ==:. 0 in
  let%hw reading = ~:done_reading &: i.upstream_ready in
  let%hw packet_ready = done_reading &: packet_queued.value in
  let rb =
    Ring_buffer.create
      scope
      reg_spec
      ~capacity:buf_capacity
      ~write_data:i.downstream_tx.tx_data
      ~write_valid:rb_write_valid.value
      ~reading
  in
  let%hw can_write = ~:(packet_queued.value) &: rb.write_ready in
  Always.(
    compile
      [ queued_length <-- next_queued_length.value
      ; when_ reading [ decr read_bits_left ]
      ; when_
          packet_ready
          [ ready_or_released_packet_length <-- queued_length.value
          ; when_
              i.consume_packet
              [ read_bits_left <-- queued_length.value
              ; packet_queued <-- gnd
              ; next_queued_length <-- zero size_bits
              ]
          ]
      ; when_
          can_write
          [ when_
              i.downstream_tx.tx_valid
              [ rb_write_valid <-- vdd
              ; next_queued_length <-- Signal.incr queued_length.value
              ]
          ; when_
              (i.flush |: (next_queued_length.value ==:. buf_capacity))
              [ packet_queued <-- vdd ]
          ]
      ]);
  { packet_ready
  ; length = uextend ready_or_released_packet_length.value ~width:16
  ; downstream_ready = can_write
  ; upstream_tx = { tx_data = rb.read_data; tx_valid = ~:done_reading }
  }
;;

module Test = struct
  module Sim = Cyclesim.With_interface (I) (O)

  let create_sim () =
    let sim = Sim.create (create (Scope.create ())) in
    Cyclesim.reset sim;
    sim
  ;;

  let inputs (sim : Sim.t) = Cyclesim.inputs sim
  let outputs (sim : Sim.t) = Cyclesim.outputs sim

  let write_byte sim byte =
    let i = inputs sim in
    i.downstream_tx.tx_data := Bits.of_unsigned_int ~width:8 byte;
    i.downstream_tx.tx_valid := Bits.vdd;
    Cyclesim.cycle sim;
    i.downstream_tx.tx_valid := Bits.gnd
  ;;

  let write_bytes sim bytes = List.iter bytes ~f:(fun b -> write_byte sim b)

  let flush sim =
    let i = inputs sim in
    i.flush := Bits.vdd;
    Cyclesim.cycle sim;
    i.flush := Bits.gnd
  ;;

  let consume (sim : Sim.t) =
    let i = inputs sim in
    i.consume_packet := Bits.vdd;
    Cyclesim.cycle sim;
    i.consume_packet := Bits.gnd
  ;;

  let read_all (sim : Sim.t) =
    let i = Cyclesim.inputs sim in
    let o = Cyclesim.outputs sim in
    i.upstream_ready := Bits.vdd;
    let bytes = ref [] in
    while Bits.to_bool !(o.upstream_tx.tx_valid) do
      bytes := Bits.to_unsigned_int !(o.upstream_tx.tx_data) :: !bytes;
      Cyclesim.cycle sim
    done;
    i.upstream_ready := Bits.gnd;
    List.rev !bytes
  ;;

  let print_state sim =
    let o = outputs sim in
    printf
      "packet_ready=%d length=%d downstream_ready=%d tx_valid=%d\n"
      (Bits.to_unsigned_int !(o.packet_ready))
      (Bits.to_unsigned_int !(o.length))
      (Bits.to_unsigned_int !(o.downstream_ready))
      (Bits.to_unsigned_int !(o.upstream_tx.tx_valid))
  ;;

  let%expect_test "write a few bytes, flush, consume, read" =
    let sim = create_sim () in
    (* Initially ready to write, no packet *)
    print_state sim;
    [%expect {| packet_ready=0 length=0 downstream_ready=0 tx_valid=0 |}];
    (* Write 3 bytes *)
    write_bytes sim [ 0xAA; 0xBB; 0xCC ];
    print_state sim;
    [%expect {| packet_ready=0 length=0 downstream_ready=1 tx_valid=0 |}];
    (* Flush *)
    flush sim;
    print_state sim;
    [%expect {| packet_ready=1 length=3 downstream_ready=0 tx_valid=0 |}];
    (* Consume *)
    consume sim;
    print_state sim;
    [%expect {| packet_ready=0 length=3 downstream_ready=1 tx_valid=1 |}];
    (* Read out *)
    let bytes = read_all sim in
    printf "read: %s\n" (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| read: 0xAA 0xBB 0xCC |}]
  ;;

  let%expect_test "zero-length flush" =
    let sim = create_sim () in
    flush sim;
    print_state sim;
    [%expect {| packet_ready=1 length=0 downstream_ready=0 tx_valid=0 |}];
    consume sim;
    print_state sim;
    (* Should immediately be ready for next packet, no reads *)
    [%expect {| packet_ready=0 length=0 downstream_ready=1 tx_valid=0 |}]
  ;;

  let%expect_test "write during read drain (double buffer behavior)" =
    let sim = create_sim () in
    let o = outputs sim in
    write_bytes sim [ 0x01; 0x02; 0x03 ];
    flush sim;
    consume sim;
    (* Now reading is happening, but we should be able to write *)
    printf "downstream_ready=%d\n" (Bits.to_unsigned_int !(o.downstream_ready));
    [%expect {| downstream_ready=1 |}];
    (* Write new data while old packet drains *)
    write_bytes sim [ 0xDD; 0xEE ];
    (* Drain the first packet *)
    let bytes = read_all sim in
    printf
      "first packet: %s\n"
      (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| first packet: 0x01 0x02 0x03 |}];
    (* Flush and consume second packet *)
    flush sim;
    print_state sim;
    [%expect {| packet_ready=1 length=2 downstream_ready=0 tx_valid=0 |}];
    consume sim;
    let bytes = read_all sim in
    printf
      "second packet: %s\n"
      (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| second packet: 0xDD 0xEE |}]
  ;;

  let%expect_test "auto-flush at max_data" =
    let sim = create_sim () in
    let o = outputs sim in
    (* Fill to max_data *)
    for byte = 0 to buf_capacity - 1 do
      write_byte sim (byte land 0xFF)
    done;
    print_state sim;
    [%expect {| packet_ready=1 length=1500 downstream_ready=0 tx_valid=0 |}];
    (* Should not accept more writes *)
    printf "downstream_ready=%d\n" (Bits.to_unsigned_int !(o.downstream_ready));
    [%expect {| downstream_ready=0 |}]
  ;;

  let%expect_test "consume while still writing does nothing" =
    let sim = create_sim () in
    write_bytes sim [ 0x01; 0x02 ];
    (* consume_packet without packet_ready should be ignored *)
    consume sim;
    print_state sim;
    [%expect {| packet_ready=0 length=0 downstream_ready=1 tx_valid=0 |}];
    (* Data should still be there *)
    flush sim;
    consume sim;
    let bytes = read_all sim in
    printf "read: %s\n" (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| read: 0x01 0x02 |}]
  ;;

  let%expect_test "single byte packet" =
    let sim = create_sim () in
    write_byte sim 0x42;
    flush sim;
    print_state sim;
    [%expect {| packet_ready=1 length=1 downstream_ready=0 tx_valid=0 |}];
    consume sim;
    let bytes = read_all sim in
    printf "read: %s\n" (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| read: 0x42 |}]
  ;;

  let%expect_test "back to back packets" =
    let sim = create_sim () in
    for packet = 0 to 2 do
      let data = List.init 4 ~f:(fun j -> (packet * 16) + j) in
      write_bytes sim data;
      flush sim;
      consume sim;
      let bytes = read_all sim in
      printf
        "packet %d: %s\n"
        packet
        (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ")
    done;
    [%expect
      {|
        packet 0: 0x00 0x01 0x02 0x03
        packet 1: 0x10 0x11 0x12 0x13
        packet 2: 0x20 0x21 0x22 0x23
        |}]
  ;;

  let%expect_test "upstream backpressure - read with ready toggling" =
    let sim = create_sim () in
    let i = inputs sim in
    let o = outputs sim in
    write_bytes sim [ 0x01; 0x02; 0x03; 0x04 ];
    flush sim;
    consume sim;
    (* Read one byte at a time with ready toggling *)
    let bytes = ref [] in
    for _ = 0 to 3 do
      bytes := Bits.to_unsigned_int !(o.upstream_tx.tx_data) :: !bytes;
      i.upstream_ready := Bits.vdd;
      Cyclesim.cycle sim;
      i.upstream_ready := Bits.gnd;
      Cyclesim.cycle sim
    done;
    let bytes = List.rev !bytes in
    printf "read: %s\n" (List.map bytes ~f:(sprintf "0x%02X") |> String.concat ~sep:" ");
    [%expect {| read: 0x01 0x02 0x03 0x04 |}]
  ;;
end
