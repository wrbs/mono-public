open! Core
open! Async
open Jsonaf.Export

module Rpcs = struct
  module Subtract = struct
    module Params = struct
      type t = int * int [@@deriving jsonaf]
    end

    module Response = struct
      type t = int [@@deriving jsonaf]
    end

    let t =
      Jsonrpc_async.Typed.Method_call.with_params
        "subtract"
        ~params:(module Params)
        ~response:(module Response)
    ;;
  end
end

let subtract_impl =
  Jsonrpc_async.Typed.Method_call.implement Rpcs.Subtract.t
  @@ fun () (a, b) -> return (a - b)
;;

let command =
  Jsonrpc_async.Streaming_lines.Stdio.command
    ~summary:"demo rpc"
    ~implementations:(Jsonrpc_async.Handler.Implementations.create_exn [ subtract_impl ])
    ~connection_state:(fun _ _ -> ())
    (Command.Param.return (return (Ok ())))
;;

let () = Command_unix.run command

(* TODO: why doesn't EOF work *)
