open! Core

module Method_call : sig
  type t =
    { method_ : string
    ; params : Jsonaf.t option
    ; id : Id.t
    }
  [@@deriving sexp_of]
end

module Notification : sig
  type t =
    { method_ : string
    ; params : Jsonaf.t option
    }
  [@@deriving sexp_of]
end

module Invalid : sig
  type t = { id : Id.t option } [@@deriving sexp_of]
end

module Call : sig
  type t =
    | Method_call of Method_call.t
    | Notification of Notification.t
    | Invalid of Invalid.t
  [@@deriving sexp_of, jsonaf]
end

type t =
  | Single of Call.t
  | Batch of Call.t list
[@@deriving sexp_of, jsonaf]

val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string
val to_string_hum : t -> string
