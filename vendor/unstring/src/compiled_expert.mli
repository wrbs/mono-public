open! Core

(** The expert interface for running the parser. *)

module Run_error : sig
  module Caught_exn : sig
    type t = [ `Caught_exn of exn ] [@@deriving sexp_of]
  end

  module Ambiguous : sig
    type 'a t = [ `Ambiguous of 'a * 'a ] [@@deriving sexp_of]
  end

  type 'a t =
    [ Caught_exn.t
    | 'a Ambiguous.t
    ]
  [@@deriving sexp_of]

  val to_error : ('a -> Sexp.t) -> [< 'a t ] -> Error.t
end

module Settings : sig
  module Parse_type : sig
    type ('a, 'b, 'err) t =
      | Id : ('a, 'a, Nothing.t) t
      | Error_handling : ('a, ('a, 'err) Result.t, 'err) t
  end

  module On_exn : sig
    type 'err t =
      | Error : [> Run_error.Caught_exn.t ] t
      | Raise : 'err t
  end

  module On_multiple_parses : sig
    type ('b, 'c, 'err) t =
      | First_parse : ('b, 'b option, 'err) t
      | Error_if_ambiguous : ('b, 'b option, [> 'b Run_error.Ambiguous.t ]) t
      | Raise_if_ambiguous : ('b -> Sexp.t) -> ('b, 'b option, 'err) t
      | All_parses : ('b, 'b list, 'err) t
      | Parse_sequence : ('b, 'b Sequence.t, 'err) t
  end

  type ('a, 'b, 'c, 'err) t =
    { parse_type : ('a, 'b, 'err) Parse_type.t
    ; on_exn : 'err On_exn.t
    ; on_multiple_parses : ('b, 'c, 'err) On_multiple_parses.t
    }
end

type 'a t

val run : 'a t -> string -> settings:('a, _, 'c, _) Settings.t -> 'c

module Private : sig
  val create : 'a Defs.Parser.t -> 'a t
end
