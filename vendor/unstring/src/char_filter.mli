open! Core

(** A filter for matching individual characters. Used both as the user-facing character
    class specification in [Unstring.t] constructors and as the terminal type in the
    parsing engine. *)
type t =
  | Any
  | Digit
  | Alpha
  | Whitespace
  | Char of char
  | Fun of (char -> bool)
  | Not of t
[@@deriving sexp_of]

val matches : t -> char -> bool
