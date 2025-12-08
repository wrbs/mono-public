open! Core
open! Async

module Packet = struct
  let magic_bytes = "SNM"

  module Kind = struct
    type t =
      | Instant
      | Reset
      | Queue
    [@@deriving sexp_of]

    let to_char = function
      | Instant -> 'i'
      | Reset -> 'r'
      | Queue -> 'q'
    ;;
  end
end

type t =
  { flush : unit -> unit
  ; shutdown : unit -> unit
  ; iobuf : (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
  ; seqnum : Protocol.Seqnum.t ref
  }

let create addr ~on_send =
  let%map socket = Socket.connect (Socket.create Socket.Type.udp) addr in
  let send_sync = Async_udp.send_sync () |> Or_error.ok_exn in
  let fd = Socket.fd socket in
  let iobuf = Iobuf.create ~len:Async_udp.default_capacity in
  let port = Socket.Address.Inet.port addr in
  let seqnum = ref Protocol.Seqnum.zero in
  let flush () =
    let length = Iobuf.length_lo iobuf in
    if length > 0
    then (
      Iobuf.flip_lo iobuf;
      let seq = !seqnum in
      [%log.global.debug
        "Flushing packet"
          (seq : Protocol.Seqnum.t)
          (length : int)
          (iobuf : (_, _, _) Iobuf.Window.Hexdump.t)];
      let result = send_sync fd (Iobuf.read_only iobuf) in
      Unix.Syscall_result.Unit.ok_or_unix_error_with_args_exn
        result
        ~syscall_name:"send"
        port
        [%sexp_of: int];
      on_send seq;
      seqnum := Protocol.Seqnum.next seq;
      Iobuf.reset iobuf)
  in
  let shutdown () = Socket.shutdown socket `Both in
  { flush; shutdown; iobuf; seqnum }
;;

let shutdown t = t.shutdown ()

let new_packet t ~kind =
  t.flush ();
  let seq = !(t.seqnum) in
  [%log.global.debug "Starting packet" (kind : Packet.Kind.t) (seq : Protocol.Seqnum.t)];
  Iobuf.Fill.stringo t.iobuf Packet.magic_bytes;
  Iobuf.Fill.char t.iobuf (Packet.Kind.to_char kind);
  Iobuf.Fill.Int_repr.uint32_be t.iobuf !(t.seqnum)
;;

let write_byte t byte ~new_packet_kind =
  if Iobuf.is_empty t.iobuf then new_packet t ~kind:new_packet_kind;
  Iobuf.Fill.char t.iobuf byte
;;

let write_u16 t v ~new_packet_kind =
  write_byte t (Byte.of_int_exn (v lsr 8)) ~new_packet_kind;
  write_byte t (Byte.of_int_exn (v land 0xFF)) ~new_packet_kind
;;

let write_messages t messages ~new_packet_kind =
  let _ =
    Collection.fold messages ~init:None ~f:(fun running_status msg ->
      Midi.Live_message.Running_status.encode
        running_status
        msg
        ~f:(write_byte t ~new_packet_kind))
  in
  ()
;;

let send_now t messages ~reset =
  new_packet t ~kind:(if reset then Reset else Instant);
  write_messages t messages ~new_packet_kind:Instant (* even if initial was reset*);
  t.flush ()
;;

let max_u16 = Int_repr.Uint16.max_value |> Int_repr.Uint16.to_base_int

let get_and_validate_payload_length messages =
  let _, payload_length =
    Collection.fold messages ~init:(None, 0) ~f:(fun (running_status, acc) event ->
      let len, running_status =
        Midi.Live_message.Running_status.length running_status event
      in
      running_status, acc + len)
  in
  if payload_length > max_u16
  then
    raise_s [%message "Payload length too large" (payload_length : int) (max_u16 : int)];
  payload_length
;;

let send_queue t entries =
  let new_packet_kind = Packet.Kind.Queue in
  let write_delta ~delta_ms =
    if delta_ms < 0 then raise_s [%message "delta_ms can't be negative"];
    let rec loop left =
      match left > max_u16 with
      | true ->
        (* insert 0-length entry and loop *)
        write_u16 t max_u16 ~new_packet_kind;
        write_u16 t 0 ~new_packet_kind;
        loop (left - max_u16)
      | false -> write_u16 t left ~new_packet_kind
    in
    loop delta_ms
  in
  new_packet t ~kind:Queue;
  Collection.iter entries ~f:(fun (~delta_ms, messages) ->
    let payload_length = get_and_validate_payload_length messages in
    write_delta ~delta_ms;
    write_u16 t payload_length ~new_packet_kind;
    write_messages t messages ~new_packet_kind);
  t.flush ()
;;
