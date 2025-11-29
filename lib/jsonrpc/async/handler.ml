open! Core
open! Async

module Implementation = struct
  module Method_call = struct
    type _ t =
      | T :
          { parse_params : Jsonaf.t option -> ('params, Jsonaf.t option) Result.t
          ; f : 'state -> 'params -> (Jsonaf.t, Jsonrpc.Rpc_error.t) Result.t Deferred.t
          }
          -> 'state t

    let lift t ~f:lift_f =
      let (T { parse_params; f }) = t in
      T { parse_params; f = (fun state -> f (lift_f state)) }
    ;;
  end

  module Notification = struct
    type _ t =
      | T :
          { parse_params : Jsonaf.t option -> 'params option
          ; f : 'state -> 'params -> unit Deferred.t
          }
          -> 'state t

    let lift t ~f:lift_f =
      let (T { parse_params; f }) = t in
      T { parse_params; f = (fun state -> f (lift_f state)) }
    ;;
  end

  type 'a t =
    | Method_call of (string * 'a Method_call.t)
    | Notification of (string * 'a Notification.t)

  let method_call ~method_ ~parse_params f = Method_call (method_, T { parse_params; f })
  let notification ~method_ ~parse_params f = Notification (method_, T { parse_params; f })

  let lift t ~f =
    match t with
    | Method_call (method_, x) -> Method_call (method_, Method_call.lift x ~f)
    | Notification (method_, x) -> Notification (method_, Notification.lift x ~f)
  ;;
end

module Implementations = struct
  type 'a t =
    { method_calls : 'a Implementation.Method_call.t String.Table.t
    ; notifications : 'a Implementation.Notification.t String.Table.t
    }

  let create (impls : _ Implementation.t list) =
    let method_calls, notifications =
      List.partition_map impls ~f:(function
        | Method_call (method_, x) -> First (method_, x)
        | Notification (method_, x) -> Second (method_, x))
    in
    let make_table alist ~kind =
      match String.Table.of_alist alist with
      | `Ok m -> Ok m
      | `Duplicate_key method_ ->
        Or_error.error_s [%message "Duplicate method" (kind : string) (method_ : string)]
    in
    let%map.Or_error method_calls = make_table method_calls ~kind:"method call"
    and notifications = make_table notifications ~kind:"notification" in
    { method_calls; notifications }
  ;;

  let create_exn impls = create impls |> Or_error.ok_exn

  let lift t ~f =
    { method_calls = Hashtbl.map t.method_calls ~f:(Implementation.Method_call.lift ~f)
    ; notifications = Hashtbl.map t.notifications ~f:(Implementation.Notification.lift ~f)
    }
  ;;

  let with_state t state = lift t ~f:(fun () -> state)
end

module Single = struct
  let handle_call
    (call : Jsonrpc.Request.Call.t)
    ~(implementations : unit Implementations.t)
    =
    match call with
    | Invalid { id } ->
      return
        (Some
           (Jsonrpc.Response.Output.Failure
              { error = Jsonrpc.Rpc_error.invalid_request; id = Jsonrpc.Id.of_opt id }))
    | Method_call { method_; params; id } ->
      (match Hashtbl.find implementations.method_calls method_ with
       | None ->
         return
           (Some
              (Jsonrpc.Response.Output.Failure
                 { error = Jsonrpc.Rpc_error.method_not_found; id }))
       | Some (T { parse_params; f }) ->
         (match Option.try_with (fun () -> parse_params params) with
          | None ->
            return
              (Some
                 (Jsonrpc.Response.Output.Failure
                    { error = Jsonrpc.Rpc_error.invalid_params; id }))
          | Some (Error data) ->
            return
              (Some
                 (Jsonrpc.Response.Output.Failure
                    { error = { Jsonrpc.Rpc_error.invalid_params with data }; id }))
          | Some (Ok x) ->
            (match%map.Eager_deferred
               Eager_deferred.Or_error.try_with (fun () -> f () x)
             with
             | Ok (Ok result) -> Some (Jsonrpc.Response.Output.Success { result; id })
             | Ok (Error error) -> Some (Jsonrpc.Response.Output.Failure { error; id })
             | Error error ->
               Some
                 (Jsonrpc.Response.Output.Failure
                    { error =
                        { code = -32000
                        ; message = "Handler raised"
                        ; data = Some (`String (Error.to_string_mach error))
                        }
                    ; id
                    }))))
    | Notification { method_; params } ->
      (match Hashtbl.find implementations.notifications method_ with
       | None -> return None
       | Some (T { parse_params; f }) ->
         (match Option.try_with_join (fun () -> parse_params params) with
          | None -> return None
          | Some x ->
            let%map.Eager_deferred (_ : unit Or_error.t) =
              Eager_deferred.Or_error.try_with (fun () -> f () x)
            in
            None))
  ;;

  let handle_request
    ?(batch_how = `Parallel)
    (request : Jsonrpc.Request.t)
    ~implementations
    =
    match request with
    | Single call ->
      let%map.Eager_deferred output_opt = handle_call call ~implementations in
      let%map.Option output = output_opt in
      Jsonrpc.Response.Single output
    | Batch calls ->
      let%map.Eager_deferred opt_outputs =
        Eager_deferred.List.map ~how:batch_how calls ~f:(handle_call ~implementations)
      in
      (match List.filter_opt opt_outputs with
       | [] -> None
       | outputs -> Some (Jsonrpc.Response.Batch outputs))
  ;;

  let handle_request' ?batch_how request ~implementations =
    match request with
    | Error `bad_json ->
      return
        (Some
           (Jsonrpc.Response.Single
              (Failure { error = Jsonrpc.Rpc_error.parse_error; id = Null })))
    | Ok request -> handle_request ?batch_how request ~implementations
  ;;

  let handle_string ?batch_how input ~implementations =
    let%map.Eager_deferred response =
      handle_request'
        ?batch_how
        ~implementations
        (match Jsonrpc.Request.of_string_opt input with
         | Some request -> Ok request
         | None -> Error `bad_json)
    in
    Option.value_map response ~default:"" ~f:Jsonrpc.Response.to_string
  ;;
end

module Multi = struct
  let handle_requests ?(how = `Parallel) ?batch_how requests ~implementations =
    let batch_how = Option.value batch_how ~default:how in
    let%map.Eager_deferred response_opts =
      Eager_deferred.List.map
        requests
        ~how
        ~f:(Single.handle_request ~batch_how ~implementations)
    in
    List.filter_opt response_opts
  ;;

  let handle_string ?how ?batch_how input ~implementations =
    let%map.Eager_deferred responses =
      match Jsonaf.parse_many input with
      | Ok jsonafs ->
        let requests = List.map jsonafs ~f:[%of_jsonaf: Jsonrpc.Request.t] in
        handle_requests ?how ?batch_how requests ~implementations
      | Error _ ->
        return
          [ Jsonrpc.Response.Single
              (Failure { error = Jsonrpc.Rpc_error.parse_error; id = Null })
          ]
    in
    List.map responses ~f:Jsonrpc.Response.to_string |> String.concat_lines
  ;;
end
