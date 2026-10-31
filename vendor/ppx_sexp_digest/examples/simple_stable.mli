(** Exposed for testing purposes in [various_payloads.ml] *)

module Stable : sig
  module V1 : sig
    type t [@@deriving bin_io, sexp_grammar, sexp]
  end
end
