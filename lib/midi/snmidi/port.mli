open! Core

module Id : sig
  type t = private string [@@deriving jsonaf]

  include String_id.S with type t := t
end

type t =
  { id : Id.t
  ; name : string
  }
[@@deriving jsonaf, sexp_of]
