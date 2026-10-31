open! Core

module Token : sig
  type t =
    | A
    | B
  [@@deriving compare, enumerate, equal, quickcheck, sexp_of]
end

module Grammar :
  Unstring_engine.S
  with type token = Token.t
   and type terminal = Token.t
   and type input = string

module Parser : module type of Unstring_engine.Parser.Make (Grammar)

val tokens_to_string : Token.t list -> string
val parse : 'a Grammar.t -> Token.t list -> 'a Sequence.t

val matching_tokens_for_grammar
  :  'a Grammar.t
  -> Token.t list Quickcheck.Generator.t option
