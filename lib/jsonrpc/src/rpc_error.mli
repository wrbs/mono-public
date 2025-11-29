open! Core

type t =
  { code : int
  ; message : string
  ; data : Jsonaf.t option
  }
[@@deriving sexp_of, jsonaf]

val create : ?data:Jsonaf.t -> int -> string -> t

(** [of_error] and [to_error] use heuristics which usually work. However, note they don't
    roundtrip and aren't intended to *)

val of_error : ?code:int -> Error.t -> t
val to_error : t -> Error.t

(** Standard defined errors *)

val parse_error : t
val invalid_request : t
val method_not_found : t
val invalid_params : t
val internal_error : t
