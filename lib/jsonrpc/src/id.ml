open! Core

type t =
  | String of string
  | Number of string
  | Null
[@@deriving sexp]

let jsonaf_of_t : t -> Jsonaf.t = function
  | String s -> `String s
  | Number n -> `Number n
  | Null -> `Null
;;

let t_of_jsonaf : Jsonaf.t -> t = function
  | `String s -> String s
  | `Number n -> Number n
  | `Null -> Null
  | _ -> raise_s [%message "Jsonrpc.ID: Expected string/number/null"]
;;

let of_opt = function
  | None -> Null
  | Some t -> t
;;
