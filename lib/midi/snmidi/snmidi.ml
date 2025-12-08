open! Core
open! Async
module Port = Port

module Problem = struct
  module Server = struct
    type t = Protocol.Error_response.t

    let to_error (t : t) =
      let message = [%string "Server error: %{t.error}"] in
      let context = t.context in
      match context with
      | [] -> Error.of_string message
      | _ -> Error.create_s [%message message (context : string list)]
    ;;

    let sexp_of_t t = [%sexp (to_error t : Error.t)]

    let pretty_ansi_format (t : t) =
      match t.ansi with
      | Some s -> s
      | None -> Error.to_string_hum (to_error t)
    ;;
  end

  type t = Protocol.Problem.t =
    | Server of Protocol.Error_response.t
    | Client of Error.t
  [@@deriving sexp_of]

  let to_error = function
    | Client error -> error
    | Server error -> Server.to_error error
  ;;

  let to_or_error = Result.map_error ~f:to_error
  let try_with_join = Protocol.Problem.try_with_join
  let error_s sexp = Client (Error.create_s sexp)
end

module Close_reason = struct
  type t =
    | Shutdown_requested
    | Problem of Problem.t
  [@@deriving sexp_of]
end

type live =
  { data_connection : Data_connection.t
  ; control_connection : Control_connection.t
  ; pending_timeouts : (Protocol.Seqnum.t * (unit, unit) Clock_ns.Event.t) Queue.t
  }

type status =
  | Live of live
  | Shutdown of Close_reason.t

type t =
  { mutable status : status
  ; closed : Close_reason.t Ivar.t
  }

let default_udp_ack_timeout = Time_ns.Span.of_int_sec 5

let abort_all_timeouts queue =
  Queue.iter queue ~f:(fun (_, event) -> Clock_ns.Event.abort_if_possible event ());
  Queue.clear queue
;;

let trigger_close_if_connected t reason =
  match t.status with
  | Shutdown _ -> (* already closed *) ()
  | Live { data_connection; control_connection; pending_timeouts } ->
    abort_all_timeouts pending_timeouts;
    (try Control_connection.shutdown control_connection with
     | _ -> ());
    (try Data_connection.shutdown data_connection with
     | _ -> ());
    t.status <- Shutdown reason;
    Ivar.fill_exn t.closed reason
;;

let close_on_error t error =
  match t.status with
  | Shutdown _ -> ()
  | Live { data_connection; _ } ->
    let () =
      try
        (* Server does this too but just in case the tcp stream is stuck so the
        server doesn't realize we've shut down, but udp is sitll working *)
        Data_connection.send_now data_connection (Midi.Live_message.panic ()) ~reset:true
      with
      | _ -> ()
    in
    trigger_close_if_connected t (Problem error)
;;

let run_if_connected t ~f =
  match t.status with
  | Shutdown _ -> ()
  | Live live ->
    (try f live with
     | exn -> close_on_error t (Client (Error.of_exn exn)))
;;

let run_or_close_reason t ~f =
  match t.status with
  | Shutdown close_reason -> Error close_reason
  | Live live ->
    (match f live with
     | x -> Ok x
     | exception exn ->
       let problem = Problem.Client (Error.of_exn exn ~backtrace:`Get) in
       close_on_error t problem;
       Error (Problem problem))
;;

let shutdown_if_connected t ~stop_all_notes =
  if not stop_all_notes
  then
    run_if_connected t ~f:(fun { control_connection; _ } ->
      Control_connection.write_exn
        control_connection
        [%jsonaf_of: Protocol.Established_command.t]
        Shutdown_without_stop);
  trigger_close_if_connected t Shutdown_requested
;;

let send t messages =
  run_or_close_reason t ~f:(fun { data_connection; _ } ->
    Data_connection.send_now data_connection messages ~reset:false)
;;

let send_if_connected t messages =
  run_if_connected t ~f:(fun { data_connection; _ } ->
    Data_connection.send_now data_connection messages ~reset:false)
;;

let get_cleanup_messages = function
  | `Don't_cleanup -> Collection.empty
  | `Stop_all_notes -> Midi.Live_message.panic ()
  | `Custom messages -> messages
;;

let reset_queue t ~cleanup =
  run_or_close_reason t ~f:(fun { data_connection; _ } ->
    Data_connection.send_now data_connection (get_cleanup_messages cleanup) ~reset:true)
;;

let reset_queue_if_connected t ~cleanup =
  run_if_connected t ~f:(fun { data_connection; _ } ->
    Data_connection.send_now data_connection (get_cleanup_messages cleanup) ~reset:true)
;;

let queue_messages t events =
  run_or_close_reason t ~f:(fun { data_connection; _ } ->
    Data_connection.send_queue data_connection events)
;;

let queue_messages_if_connected t events =
  run_if_connected t ~f:(fun { data_connection; _ } ->
    Data_connection.send_queue data_connection events)
;;

let create_exn ~timeout ~control_connection ~udp_addr =
  let timeout = Option.value timeout ~default:default_udp_ack_timeout in
  let pending_timeouts = Queue.create () in
  let timed_out = Ivar.create () in
  let on_send seqnum =
    let event =
      Clock_ns.Event.run_after
        timeout
        (fun () ->
          Ivar.fill_if_empty timed_out seqnum;
          abort_all_timeouts pending_timeouts)
        ()
    in
    Queue.enqueue pending_timeouts (seqnum, event)
  in
  let%map data_connection = Data_connection.create udp_addr ~on_send in
  let closed = Ivar.create () in
  let t =
    { status = Live { data_connection; control_connection; pending_timeouts }; closed }
  in
  Deferred.upon (Ivar.read timed_out) (fun seqnum ->
    close_on_error
      t
      (Problem.Client
         (Error.of_lazy_sexp
            [%lazy_message
              "Timed out waiting for ack"
                (seqnum : Protocol.Seqnum.t)
                (timeout : Time_ns.Span.t)])));
  t
;;

let ack_handler t =
  let%map error =
    Deferred.repeat_until_finished () (fun () ->
      match t.status with
      | Shutdown _ -> return (`Finished None)
      | Live { control_connection; pending_timeouts; _ } ->
        (match%map
           Control_connection.read_opt control_connection [%of_jsonaf: Protocol.Ack.t]
         with
         | Error problem -> `Finished (Some problem)
         | Ok None ->
           (match t.status with
            | Live _ ->
              `Finished (Some (Problem.error_s [%message "Unexpected EOF from server"]))
            | Shutdown _ ->
              (* it shut down while we were waiting for the ack, which is fine *)
              `Finished None)
         | Ok (Some { ack }) ->
           (match Queue.dequeue pending_timeouts with
            | None ->
              `Finished
                (Some
                   (Problem.error_s
                      [%message
                        "Got unexpected ack, wasn't expecting any"
                          (ack : Protocol.Seqnum.t)]))
            | Some (expected, event) ->
              (match [%equal: Protocol.Seqnum.t] ack expected with
               | false ->
                 `Finished
                   (Some
                      (Problem.error_s
                         [%message
                           "Got unexpected ack, wasn't expecting any"
                             ~got_seq:(ack : Protocol.Seqnum.t)
                             (expected : Protocol.Seqnum.t)]))
               | true ->
                 Clock_ns.Event.abort_if_possible event ();
                 `Repeat ()))))
  in
  match error with
  | None -> ()
  | Some error -> close_on_error t error
;;

let connect' ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port =
  Problem.try_with_join
  @@ fun () ->
  (* We want to catch writer errors after initialization by running in a custom
     monitor (assuming we decide to connect). We keep it attached to the parent
     (try_with_join) to start with though. *)
  let monitor = Monitor.create () in
  Scheduler.within' ~monitor
  @@ fun () ->
  let%bind _socket, reader, writer =
    Tcp.connect (Tcp.Where_to_connect.of_inet_address addr)
  in
  let control_connection = Control_connection.create reader writer in
  let next_error = Monitor.get_next_error monitor in
  Deferred.upon next_error (fun _ -> Control_connection.shutdown control_connection);
  let%bind.Deferred.Result { ports } =
    Control_connection.call
      (module Protocol.Initialize)
      ?timeout:connect_timeout
      control_connection
      { version = 0; client_name }
  in
  match%bind pick_port ports with
  | `Don't_connect abort_value ->
    Control_connection.shutdown control_connection;
    return (Ok (`Aborted abort_value))
  | `Connect (id, res) ->
    let%bind.Deferred.Result { udp_port } =
      Control_connection.call
        (module Protocol.Midi_connect)
        ?timeout:connect_timeout
        control_connection
        { id }
    in
    let%map t =
      create_exn
        ~timeout:udp_ack_timeout
        ~control_connection
        ~udp_addr:
          (Socket.Address.Inet.create (Socket.Address.Inet.addr addr) ~port:udp_port)
    in
    (* At this point, we can detach the monitor *)
    Monitor.detach monitor;
    Deferred.upon next_error (fun exn -> close_on_error t (Client (Error.of_exn exn)));
    don't_wait_for (ack_handler t);
    Ok (`Connected (t, res))
;;

let connect ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port =
  match%map.Deferred.Result
    connect' ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port:(fun ports ->
      match%map pick_port ports with
      | `Connect id -> `Connect (id, ())
      | `Don't_connect x -> `Don't_connect x)
  with
  | `Aborted x -> `Aborted x
  | `Connected (t, ()) -> `Connected t
;;

let connect_or_error ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port =
  match%map
    connect ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port:(fun ports ->
      match%map pick_port ports with
      | Ok id -> `Connect id
      | Error error -> `Don't_connect error)
  with
  | Error problem -> Error (Problem.to_error problem)
  | Ok (`Connected t) -> Ok t
  | Ok (`Aborted error) -> Error error
;;

let list_ports ?connect_timeout addr ~client_name =
  match%map.Deferred.Result
    connect ?connect_timeout addr ~client_name ~pick_port:(fun ports ->
      return (`Don't_connect ports))
  with
  | `Connected _ -> failwith "unreachable"
  | `Aborted ports -> ports
;;

let with_
  ?connect_timeout
  ?udp_ack_timeout
  ?don't_stop_notes_on_shutdown
  ~addr
  ~client_name
  f
  =
  match%bind.Deferred.Result
    connect' ?connect_timeout ?udp_ack_timeout addr ~client_name ~pick_port:f
  with
  | `Aborted result -> return (Ok result)
  | `Connected (t, f) ->
    let outcome = Deferred.Or_error.try_with ~extract_exn:false (fun () -> f t) in
    choose
      [ choice (Ivar.read t.closed) (fun close_reason ->
          match close_reason with
          | Problem problem -> Error problem
          | Shutdown_requested ->
            Error (Problem.error_s [%message "Shutdown requested manually"]))
      ; choice outcome (function
          | Ok result ->
            shutdown_if_connected
              t
              ~stop_all_notes:
                (match don't_stop_notes_on_shutdown with
                 | Some () -> false
                 | None -> true);
            Ok result
          | Error error ->
            close_on_error t (Client error);
            Error (Problem.Client error))
      ]
;;

let is_connected t = Ivar.is_empty t.closed
let close_finished t = Ivar.read t.closed
