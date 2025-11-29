open! Core

module Success : sig
  type t =
    { result : Jsonaf.t
    ; id : Id.t
    }
  [@@deriving sexp_of]
end

module Failure : sig
  type t =
    { error : Rpc_error.t
    ; id : Id.t
    }
  [@@deriving sexp_of]
end

module Output : sig
  type t =
    | Success of Success.t
    | Failure of Failure.t
  [@@deriving sexp_of, jsonaf]
end

type t =
  | Single of Output.t
  | Batch of Output.t list
[@@deriving sexp_of, jsonaf]

val of_string : string -> t
val of_string_opt : string -> t option
val to_string : t -> string
val to_string_hum : t -> string
