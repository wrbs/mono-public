open! Core

type t =
  | String of string
  | Number of string
  | Null
[@@deriving sexp, jsonaf]

val of_opt : t option -> t
