open! Core
open! Async
module Problem = Protocol.Problem

let default_timeout = Time_ns.Span.of_int_sec 2

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; shutdown_called : unit Set_once.t
  }

let create reader writer = { reader; writer; shutdown_called = Set_once.create () }

let read_opt t of_json =
  Problem.try_with_join
  @@ fun () ->
  match%map Reader.read_line t.reader with
  | `Eof -> Ok None
  | `Ok line ->
    (match Jsonaf.of_string line with
     | exception exn ->
       Problem.error_s [%message "Invalid json" (exn : exn) (line : string)]
     | json ->
       (match [%of_jsonaf: Protocol.Error_response.t] json with
        | error -> Error (Problem.Server error)
        | exception _ ->
          (match of_json json with
           | exception exn ->
             Problem.error_s [%message "Json parse error" (exn : exn) (json : Jsonaf.t)]
           | value -> Ok (Some value))))
;;

let read reader of_json ~waiting_for =
  let%map result = read_opt reader of_json in
  match%bind.Result result with
  | Some x -> Ok x
  | None -> Problem.error_s [%message "Unexpected EOF" (waiting_for : string)]
;;

let write_exn t json_of x = Writer.write_line t.writer (Jsonaf.to_string (json_of x))

let call
  (type request response)
  (module M : Protocol.Call with type Request.t = request and type Response.t = response)
  ?(timeout = default_timeout)
  t
  arg
  =
  Problem.try_with_join
  @@ fun () ->
  let operation = M.desc in
  [%log.global.debug "Sending control message" (arg : M.Request.t)];
  let () = write_exn t [%jsonaf_of: M.Request.t] arg in
  let%bind () = Writer.flushed t.writer in
  match%map
    Clock_ns.with_timeout
      timeout
      (read t [%of_jsonaf: M.Response.t] ~waiting_for:[%string "%{operation} response"])
  with
  | `Result result ->
    [%log.global.debug "Read control message" (result : (M.Response.t, Problem.t) result)];
    result
  | `Timeout ->
    Problem.error_s
      [%message
        "Timeout while waiting for response"
          (operation : string)
          (arg : M.Request.t)
          (timeout : Time_ns.Span.t)]
;;

let shutdown t =
  Set_once.get_or_set_thunk t.shutdown_called ~f:(fun () ->
    don't_wait_for
      (let%bind () = Writer.close t.writer in
       Reader.close t.reader))
;;
