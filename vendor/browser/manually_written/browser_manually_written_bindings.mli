module Unsafe : sig
  module Error : sig
    type t = Not_in_correct_context

    val sexp_of_t : t -> Sexplib.Sexp.t
    val t_of_sexp : Sexplib.Sexp.t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
  end

  type 'a t = ('a, Error.t) result
end

module Context : sig
  type 'a t =
    | Secure : [ `Secure ] t
    | Cross_origin_isolated : [ `Cross_origin_isolated | `Secure ] t
    (** [CrossOriginIsolated] IS a secure context *)
end

module Unsafe_context_checker : sig
  module type S_input = sig
    type global

    val is_in_context : 'a. context:'a Context.t -> global -> bool
    val get_global : unit -> global
  end

  type t

  val make : (module S_input) -> t
  val check : context:'a Context.t -> t -> bool
end
