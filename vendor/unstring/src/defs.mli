open! Core

module Grammar :
  Unstring_engine.S
  with type token = char
   and type terminal = Char_filter.t
   and type input = string

module Parser : module type of Unstring_engine.Parser.Make (Grammar)
