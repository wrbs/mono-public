open! Core
open! Async
module Jsonafable = Jsonaf.Jsonafable

module Params = struct
  type _ t =
    | No_params : unit t
    | Json : (module Jsonafable.S with type t = 'a) -> 'a t

  let parse (type a) (t : a t) json : (a, Jsonaf.t option) Result.t =
    match t with
    | No_params ->
      (match json with
       | None | Some `Null -> Ok ()
       | _ -> Error (Some (`String "Expected no params")))
    | Json (module M) ->
      let json = Option.value json ~default:`Null in
      (match Or_error.try_with (fun () -> [%of_jsonaf: M.t] json) with
       | Ok x -> Ok x
       | Error error ->
         Error
           (Some (`String [%string "JSON parse error: %{Error.to_string_mach error}"])))
  ;;

  let parse_opt t json = Result.ok (parse t json)

  let encode (type a) (t : a t) (x : a) =
    match t with
    | No_params -> None
    | Json (module M) -> Some ([%jsonaf_of: M.t] x)
  ;;
end

module Method_call = struct
  type ('params, 'response) t =
    { method_ : string
    ; params : 'params Params.t
    ; response : (module Jsonafable.S with type t = 'response)
    }

  let encode_response
    (type a)
    { response = (module M : Jsonafable.S with type t = a); _ }
    (response : a)
    =
    [%jsonaf_of: M.t] response
  ;;

  let no_params method_ ~response = { method_; params = No_params; response }
  let with_params method_ ~params ~response = { method_; params = Json params; response }

  let implement_result t f =
    Handler.Implementation.method_call
      ~method_:t.method_
      ~parse_params:(Params.parse t.params)
      (fun state params -> f state params >>|? encode_response t)
  ;;

  let implement_or_error ?error_code t f =
    implement_result t (fun state params ->
      f state params >>| Result.map_error ~f:(Jsonrpc.Rpc_error.of_error ?code:error_code))
  ;;

  let implement t f =
    implement_result t (fun state params -> f state params >>| Result.return)
  ;;

  let to_call t x ~id : Jsonrpc.Request.Method_call.t =
    { method_ = t.method_; params = Params.encode t.params x; id }
  ;;

  module Full_error_kind = struct
    type t =
      | Rpc_error of Jsonrpc.Rpc_error.t
      | Problem_parsing_response of Error.t
      | Problem_receiving_response of Error.t
  end

  let parse_response (type a) t json =
    let (module M : Jsonafable.S with type t = a) = t.response in
    Or_error.try_with (fun () -> M.t_of_jsonaf json)
  ;;

  let dispatch_error_kind t conn x =
    let%map.Deferred.Or_error result =
      Connection.Untyped.call_method'' ?params:(Params.encode t.params x) conn t.method_
    in
    match%map result with
    | Error error -> Error (Full_error_kind.Problem_parsing_response error)
    | Ok (Error rpc_error) -> Error (Rpc_error rpc_error)
    | Ok (Ok json) ->
      (match parse_response t json with
       | Error error -> Error (Problem_parsing_response error)
       | Ok result -> Ok result)
  ;;

  let dispatch_result t conn x =
    let%bind.Deferred.Or_error result = dispatch_error_kind t conn x in
    match%map result with
    | Ok response -> Ok (Ok response)
    | Error (Rpc_error rpc_error) -> Ok (Error rpc_error)
    | Error (Problem_receiving_response error) -> Error error
    | Error (Problem_parsing_response error) ->
      Or_error.error_s [%message "Error parsing response" (error : Error.t)]
  ;;

  let dispatch t conn x =
    match%map dispatch_result t conn x with
    | Ok (Ok response) -> Ok response
    | Ok (Error rpc_error) -> Error (Jsonrpc.Rpc_error.to_error rpc_error)
    | Error error -> Error error
  ;;
end

module Notification = struct
  type 'params t =
    { method_ : string
    ; params : 'params Params.t
    }

  let no_params method_ = { method_; params = No_params }
  let with_params method_ ~params = { method_; params = Json params }

  let implement t f =
    Handler.Implementation.notification
      ~method_:t.method_
      ~parse_params:(Params.parse_opt t.params)
      f
  ;;

  let to_call t x : Jsonrpc.Request.Notification.t =
    { method_ = t.method_; params = Params.encode t.params x }
  ;;

  let dispatch t conn x =
    Connection.Untyped.send_notification conn t.method_ ?params:(Params.encode t.params x)
  ;;
end
