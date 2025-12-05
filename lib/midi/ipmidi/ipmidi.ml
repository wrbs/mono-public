open! Core
open! Async

module Port = struct
  type t =
    | Port1
    | Port2
    | Port3
    | Port4
    | Port5
    | Port6
    | Port7
    | Port8
    | Port9
    | Port10
    | Port11
    | Port12
    | Port13
    | Port14
    | Port15
    | Port16
    | Port17
    | Port18
    | Port19
    | Port20
  [@@deriving enumerate, compare ~localize, equal ~localize, bin_io ~localize, hash]

  let index = function
    | Port1 -> 0
    | Port2 -> 1
    | Port3 -> 2
    | Port4 -> 3
    | Port5 -> 4
    | Port6 -> 5
    | Port7 -> 6
    | Port8 -> 7
    | Port9 -> 8
    | Port10 -> 9
    | Port11 -> 10
    | Port12 -> 11
    | Port13 -> 12
    | Port14 -> 13
    | Port15 -> 14
    | Port16 -> 15
    | Port17 -> 16
    | Port18 -> 17
    | Port19 -> 18
    | Port20 -> 19
  ;;

  let of_index = function
    | 0 -> Some Port1
    | 1 -> Some Port2
    | 2 -> Some Port3
    | 3 -> Some Port4
    | 4 -> Some Port5
    | 5 -> Some Port6
    | 6 -> Some Port7
    | 7 -> Some Port8
    | 8 -> Some Port9
    | 9 -> Some Port10
    | 10 -> Some Port11
    | 11 -> Some Port12
    | 12 -> Some Port13
    | 13 -> Some Port14
    | 14 -> Some Port15
    | 15 -> Some Port16
    | 16 -> Some Port17
    | 17 -> Some Port18
    | 18 -> Some Port19
    | 19 -> Some Port20
    | _ -> None
  ;;

  let to_int t = index t + 1
  let of_int n = of_index (n - 1)

  let of_int_exn n =
    match of_int n with
    | None -> raise_s [%message "Ipmidi.Port.of_int_exn: out of range 1-20" (n : int)]
    | Some t -> t
  ;;

  let to_string t = t |> to_int |> Int.to_string
  let of_string s = s |> Int.of_string |> of_int_exn

  include functor Sexpable.Of_stringable [@modality portable]

  let module_name = "Ipmidi.Port"

  include functor Identifiable.Make [@mode local] [@modality portable]

  let first_port = 21928
  let port t = index t + first_port
  let ipmidi_addr = Unix.Inet_addr.of_string "225.0.0.37"
  let sockaddr t = Socket.Address.Inet.create ipmidi_addr ~port:(port t)

  let arg_type =
    (Command.Arg_type.create_with_additional_documentation [@modality portable])
      of_string
      ~additional_documentation:(lazy "range 1-20")
  ;;
end

module Port_sender = struct
  type t =
    { port : Port.t
    ; fd : Fd.t
    ; send_sync :
        Fd.t -> (read, Iobuf.seek, Iobuf.global) Iobuf.t -> Unix.Syscall_result.Unit.t
    ; buffer : (read_write, Iobuf.seek, Iobuf.global) Iobuf.t
    ; mutable running_status : Midi.Message.Status.t option
    ; shutdown : unit -> unit
    }

  let create' ~port ~send_sync =
    let%map socket =
      Socket.connect (Socket.create Socket.Type.udp) (Port.sockaddr port)
    in
    let fd = Socket.fd socket in
    (* Small buffer for MIDI messages (3 bytes for most messages) *)
    let buffer = Iobuf.create ~len:Async_udp.default_capacity in
    let shutdown () = Socket.shutdown socket `Both in
    { port; fd; send_sync; buffer; running_status = None; shutdown }
  ;;

  let create port =
    let send_sync = Async_udp.send_sync () |> Or_error.ok_exn in
    create' ~port ~send_sync
  ;;

  let flush_exn t =
    if Iobuf.length_lo t.buffer > 0
    then (
      Iobuf.flip_lo t.buffer;
      let result = t.send_sync t.fd (Iobuf.read_only t.buffer) in
      Unix.Syscall_result.Unit.ok_or_unix_error_with_args_exn
        result
        ~syscall_name:"send"
        t.port
        [%sexp_of: Port.t];
      Iobuf.reset t.buffer;
      t.running_status <- None)
    else ()
  ;;

  let write_exn t message =
    let f () =
      t.running_status
      <- Midi.Live_message.Running_status.encode t.running_status message ~f:(fun byte ->
           Iobuf.Fill.char t.buffer byte)
    in
    match f () with
    | () -> ()
    | exception _ ->
      (* if full, flush and retry.
        
        if second write still raises, propagate it *)
      (* TODO: if we need it, splitting sysex into multiple packets if > capacity from MTU *)
      flush_exn t;
      f ()
  ;;

  let write_all_exn t messages = Collection.iter messages ~f:(write_exn t)

  let write_and_flush_exn t messages =
    write_all_exn t messages;
    flush_exn t
  ;;

  let close (t @ unique) = t.shutdown ()
end

module Sender = struct
  type t = { ports : Port_sender.t iarray }

  let create () =
    let send_sync = Async_udp.send_sync () |> Or_error.ok_exn in
    let%map port_list =
      Deferred.List.map Port.all ~how:`Parallel ~f:(fun port ->
        Port_sender.create' ~port ~send_sync)
    in
    { ports = Iarray.of_list port_list }
  ;;

  let sender t ~port = t.ports.:(Port.index port)
  let write_exn t message ~port = Port_sender.write_exn (sender t ~port) message
  let write_all_exn t messages ~port = Port_sender.write_all_exn (sender t ~port) messages

  let write_and_flush_exn t messages ~port =
    Port_sender.write_and_flush_exn (sender t ~port) messages
  ;;

  let flush_exn t = Iarray.iter t.ports ~f:Port_sender.flush_exn
  let flush_port_exn t ~port = Port_sender.flush_exn (sender t ~port)
  let close (t @ unique) = Iarray.Unique.iter t.ports ~f:Port_sender.close
end
