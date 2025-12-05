open! Core

type t =
  | Value of Value.t
  | Status of Status.t
[@@deriving sexp_of, compare ~localize, equal ~localize, enumerate]

let to_byte = function
  | Value v -> Value.to_byte v
  | Status s -> Status.to_byte s
;;

let of_byte b =
  match Value.of_byte b with
  | Some v -> Value v
  | None -> Status (Status.of_byte_exn b)
;;
