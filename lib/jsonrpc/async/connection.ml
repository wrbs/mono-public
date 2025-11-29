open! Core
open! Async

module State = struct
  module Open = struct
    type t =
      { mutable current_id : int
      ; send_request : Jsonrpc.Request.t -> unit Or_error.t Deferred.t
      ; on_close : reason:Error.t option -> unit Deferred.t
      ; pending_calls :
          (Jsonaf.t, Jsonrpc.Rpc_error.t) Result.t Or_error.t Ivar.t Int.Table.t
      }

    let next_id t =
      let current = t.current_id in
      t.current_id <- current + 1;
      current
    ;;

    let fill_ivar t ~(id : Jsonrpc.Id.t) ~value =
      let ivar_opt =
        match id with
        | Null | String _ -> None
        | Number s ->
          let%bind.Option n = Int.of_string_opt s in
          Hashtbl.find_and_remove t.pending_calls n
      in
      match ivar_opt with
      | None -> Or_error.error_s [%message "Unexpected ID" (id : Jsonrpc.Id.t)]
      | Some ivar ->
        Ivar.fill_exn ivar value;
        Ok ()
    ;;
  end

  type t =
    | Open of Open.t
    | Closed of { error : Error.t }
end

type t =
  { mutable state : State.t
  ; close_finished : unit Ivar.t
  }

let require_open t =
  match t.state with
  | Open x -> Ok x
  | Closed { error } -> Error error
;;

let handle_response t (response : Jsonrpc.Response.t) =
  let%bind.Or_error open_ = require_open t in
  match response with
  | Batch _ ->
    (* we never send batch requests *)
    Or_error.error_s [%message "Unexpected batch response"]
  | Single output ->
    (match output with
     | Success { result; id } -> State.Open.fill_ivar open_ ~id ~value:(Ok (Ok result))
     | Failure { error; id } -> State.Open.fill_ivar open_ ~id ~value:(Ok (Error error)))
;;

let close_finished t = Ivar.read t.close_finished

let close ?reason t =
  match t.state with
  | Closed _ -> close_finished t
  | Open open_ ->
    let error =
      match reason with
      | None -> Error.of_string "Connection closed"
      | Some reason -> Error.create_s [%message "Connection closed" (reason : Error.t)]
    in
    Hashtbl.iter open_.pending_calls ~f:(fun ivar -> Ivar.fill_exn ivar (Error error));
    Hashtbl.clear open_.pending_calls;
    t.state <- Closed { error };
    let%map () = open_.on_close ~reason in
    Ivar.fill_exn t.close_finished ()
;;

module Untyped = struct
  let format_id id = Jsonrpc.Id.Number (Int.to_string id)

  let call_method'' ?params t method_ =
    let%bind.Eager_deferred.Or_error open_ = return (require_open t) in
    let id = State.Open.next_id open_ in
    let ivar = Ivar.create () in
    Hashtbl.add_exn open_.pending_calls ~key:id ~data:ivar;
    let%map.Deferred.Or_error () =
      open_.send_request (Single (Method_call { method_; params; id = format_id id }))
    in
    Ivar.read ivar
  ;;

  let call_method' ?params t method_ =
    call_method'' ?params t method_ |> Deferred.Or_error.join
  ;;

  let call_method ?params t method_ =
    match%map call_method' ?params t method_ with
    | Ok (Ok x) -> Ok x
    | Error error -> Error error
    | Ok (Error rpc_error) -> Error (Jsonrpc.Rpc_error.to_error rpc_error)
  ;;

  let send_notification ?params t method_ =
    let%bind.Eager_deferred.Or_error open_ = return (require_open t) in
    open_.send_request (Single (Notification { method_; params }))
  ;;
end

module Expert = struct
  let create ~send_request ~on_close =
    let t =
      { state =
          Open
            { current_id = 0
            ; pending_calls = Int.Table.create ()
            ; on_close
            ; send_request
            }
      ; close_finished = Ivar.create ()
      }
    in
    t, `handle_response (handle_response t)
  ;;
end
