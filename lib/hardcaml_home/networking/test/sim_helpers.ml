open! Core
open! Hardcaml
open! Hardcaml_home_networking
include Hardcaml_home_test_helpers

let sfd =
  let open Bits in
  (* ugh bit incoding... *)
  let preamble = of_bit_string "01" in
  let sfd = of_bit_string "11" in
  repeat preamble ~count:4 (* msb so last out *) @: sfd @: repeat preamble ~count:3
;;

let with_sfd bits =
  let open Bits in
  sfd @: bits
;;

let skip_to_stream_start
  ?(timeout = 10000)
  sim
  ~stream_before_cycle:(stream : Bits.t ref Packet_stream.t)
  ~update_inputs
  =
  Cyclesim.with_timeout
    sim
    ~message:"timeout skipping to stream start"
    ~timeout
    ~f:(fun sim ->
      let rec loop () =
        match Bits.to_bool !(stream.start) with
        | false ->
          update_inputs sim;
          Cyclesim.cycle sim;
          loop ()
        | true -> ()
      in
      loop ())
;;

let consume_stream_result
  ?(timeout = 10000)
  sim
  ~stream_before_cycle:(stream : Bits.t ref Packet_stream.t)
  ~valid_before_cycle:valid
  ~update_inputs
  =
  let buffer = Buffer.create 1024 in
  let result =
    Cyclesim.with_timeout
      sim
      ~message:"timeout skipping to stream start"
      ~timeout
      ~f:(fun sim ->
        let rec loop () =
          if Bits.to_bool !(stream.abort)
          then `Abort
          else (
            let () =
              if Bits.to_bool !valid
              then Buffer.add_char buffer (Bits.to_char !(stream.data))
            in
            let stop = Bits.to_bool !(stream.stop) in
            match stop with
            | true -> `Stop
            | false ->
              update_inputs sim;
              Cyclesim.cycle sim;
              loop ())
        in
        loop ())
  in
  Buffer.contents buffer, result
;;

let consume_stream ?timeout sim ~stream_before_cycle ~valid_before_cycle ~update_inputs =
  let out, result =
    consume_stream_result
      ?timeout
      sim
      ~stream_before_cycle
      ~valid_before_cycle
      ~update_inputs
  in
  match result with
  | `Stop -> out
  | `Abort ->
    (* For wavetables: *)
    update_inputs sim;
    Cyclesim.cycle sim;
    raise_s [%message "Stream aborted" ~before_abort:(out : String.Hexdump.t)]
;;

module Queue_driver = struct
  module Elem = struct
    type t =
      | Wait of int
      | Call of (unit -> unit)
  end

  type t =
    { mutable cur : Elem.t or_null
    ; queue : Elem.t Queue.t
    }

  let create () = { cur = Null; queue = Queue.create () }
  let wait t n = if n >= 1 then Queue.enqueue t.queue (Wait n)
  let add t f = Queue.enqueue t.queue (Call f)

  let add_step t f ~reset =
    add t f;
    wait t 1;
    add t reset
  ;;

  let update_inputs t =
    let rec loop () =
      if Or_null.is_null t.cur then t.cur <- Queue.dequeue_or_null t.queue;
      Or_null.iter t.cur ~f:(fun cur ->
        t.cur <- Null;
        match cur with
        | Call f ->
          f ();
          loop ()
        | Wait 0 -> loop ()
        | Wait n -> t.cur <- This (Wait (n - 1)))
    in
    loop ()
  ;;
end

module Ethernet_driver = struct
  let gap = 48

  type t =
    { i : Bits.t ref Rmii.Hw.Rx.t
    ; driver : Queue_driver.t
    }

  let create i =
    let driver = Queue_driver.create () in
    Queue_driver.wait driver 1;
    { i; driver }
  ;;

  let add_packet t ~packet =
    Bits.split_msb packet ~exact:false ~part_width:8
    |> List.concat_map ~f:(Bits.split_lsb ~exact:true ~part_width:2)
    |> List.iter ~f:(fun data ->
      Queue_driver.add t.driver (fun () ->
        t.i.crsdv := Bits.vdd;
        t.i.rxd := data);
      Queue_driver.wait t.driver 1);
    Queue_driver.add t.driver (fun () ->
      t.i.rxd := Bits.zero 2;
      t.i.crsdv := Bits.gnd);
    Queue_driver.wait t.driver (gap + 1)
  ;;

  let update_inputs t = Queue_driver.update_inputs t.driver
end
