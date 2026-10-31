open! Core

module Token_lookup = struct
  type 'a t = 'a iarray [@@deriving sexp_of]

  let n = lazy (List.length Char.all)
  let create f = Iarray.init (force n) ~f:(fun i -> f (Char.of_int_exn i))
  let find t c = t.:(Char.to_int c)
end

module Grammar = struct
  type token = char [@@deriving sexp_of]
  type terminal = Char_filter.t [@@deriving sexp_of]
  type input = string [@@deriving sexp_of]

  let matches = Char_filter.matches

  module Token_lookup = Token_lookup
  include functor Unstring_engine.Make_derived
end

module Parser = Unstring_engine.Parser.Make (Grammar)
