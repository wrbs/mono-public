open! Core

(* A helper type for getting structure out of errors *)

module Payload : sig
  type t = private
    | Single of Sexp.t
    | Tags of (string * Sexp.t) list
    | Other of Sexp.t list
  [@@deriving sexp_of]
end

module Message : sig
  type t = private
    | Message of
        { message : string
        ; payload : Payload.t option
        }
    | Other of Sexp.t list
  [@@deriving sexp_of]
end

type t =
  | Message of Message.t
  | Wrapped of Message.t * t
  | Multi of t list
  | With_backtrace of t * string list
[@@deriving sexp_of]

val of_info : Info.t -> t
val of_error : Error.t -> t
val to_json : t -> Jsonaf.t
val to_message_and_data : t -> string * Jsonaf.t option
