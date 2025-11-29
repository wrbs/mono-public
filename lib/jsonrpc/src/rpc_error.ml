open! Core
open Jsonaf.Export

type t =
  { code : int
  ; message : string
  ; data : Jsonaf.t option [@jsonaf.option] [@sexp.option]
  }
[@@deriving jsonaf, sexp_of]

let create ?data code message = { code; message; data }

let of_error ?(code = 0) error =
  let message, data =
    Structured_error.to_message_and_data (Structured_error.of_error error)
  in
  { code; message; data }
;;

let to_error { code; message; data } =
  match data with
  | None -> Error.create_s [%message "JSON-RPC error" (code : int) (message : string)]
  | Some data ->
    Error.create_s
      [%message
        "JSON-RPC error"
          (code : int)
          (message : string)
          ~data:(Json_to_sexp.convert data : Sexp.t)]
;;

let parse_error = create (-32700) "Parse error"
let invalid_request = create (-32600) "Invalid Request"
let method_not_found = create (-32601) "Method not found"
let invalid_params = create (-32602) "Invalid params"
let internal_error = create (-32603) "Internal error"
