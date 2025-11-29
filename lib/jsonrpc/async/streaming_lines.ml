open! Core
open! Async

module Input = struct
  type t =
    | Request of (Jsonrpc.Request.t, [ `bad_json ]) Result.t
    | Response of Jsonrpc.Response.t

  let of_string s =
    [%log.global.debug_string "got message: %{s}"];
    match Jsonaf.parse s with
    | Error _ -> Request (Error `bad_json)
    | Ok json ->
      (match [%of_jsonaf: Jsonrpc.Response.t] json with
       | response -> Response response
       | exception _ -> Request (Ok ([%of_jsonaf: Jsonrpc.Request.t] json)))
  ;;
end

let create
  ?batch_how
  ?(implementations = Handler.Implementations.create_exn [])
  ?(on_close = fun ~reason:_ -> return ())
  reader
  writer
  ~connection_state
  =
  let write_line line =
    [%log.global.debug_string "sent message: %{line}"];
    Writer.write_line writer line
  in
  let conn, `handle_response handle_response =
    Connection.Expert.create
      ~send_request:(fun request ->
        match Writer.is_open writer with
        | true ->
          write_line (Jsonrpc.Request.to_string request);
          return (Ok ())
        | false -> Deferred.Or_error.error_s [%message "Writer closed"])
      ~on_close
  in
  let implementations =
    Handler.Implementations.with_state implementations (connection_state conn)
  in
  Deferred.upon (Reader.close_finished reader) (fun () ->
    Connection.close conn ~reason:(Error.of_string "reader closed") |> don't_wait_for);
  Deferred.upon (Writer.close_finished writer) (fun () ->
    Connection.close conn ~reason:(Error.of_string "writer closed") |> don't_wait_for);
  let rec main_loop () =
    match%bind.Eager_deferred Reader.read_line reader with
    | `Eof ->
      Connection.close conn ~reason:(Error.of_string "EOF") |> don't_wait_for;
      return ()
    | `Ok "" -> main_loop ()
    | `Ok line ->
      (match Input.of_string line with
       | Request request ->
         don't_wait_for
           (match%map.Eager_deferred
              Handler.Single.handle_request' ?batch_how request ~implementations
            with
            | Some response ->
              if Writer.is_open writer
              then write_line (Jsonrpc.Response.to_string response)
            | None -> ())
       | Response response ->
         (match handle_response response with
          | Ok () -> ()
          | Error _ -> ()));
      main_loop ()
  in
  don't_wait_for (main_loop ());
  conn
;;

module Stdio = struct
  let command' ?batch_how ?readme ~summary get_implementations_param =
    Command.async_or_error ?readme ~summary
    @@
    let%map_open.Command () = Log.Global.set_level_via_param ()
    and get_implementatons = get_implementations_param in
    fun () ->
      let%bind.Deferred.Or_error implementations = get_implementatons () in
      let conn =
        create
          ?batch_how
          ~implementations
          (Lazy.force Reader.stdin)
          (Lazy.force Writer.stdout)
          ~connection_state:Fn.id
      in
      let%map () = Connection.close_finished conn in
      Ok ()
  ;;

  let command
    ?batch_how
    ?readme
    ~summary
    ~implementations
    ~connection_state
    get_state_param
    =
    command'
      ?batch_how
      ?readme
      ~summary
      (let%map_open.Command get_state' = get_state_param in
       fun () ->
         let%map.Deferred.Or_error state = get_state' in
         Handler.Implementations.lift implementations ~f:(fun conn ->
           connection_state state conn))
  ;;
end
