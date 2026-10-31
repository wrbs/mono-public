open! Core

type t =
  | Any
  | Digit
  | Alpha
  | Whitespace
  | Char of char
  | Fun of (char -> bool)
  | Not of t
[@@deriving sexp_of]

let rec matches (t : t) (c : char) =
  match t with
  | Any -> true
  | Digit -> Char.is_digit c
  | Alpha -> Char.is_alpha c
  | Whitespace -> Char.is_whitespace c
  | Char ch -> Char.equal c ch
  | Fun f -> f c
  | Not t -> not (matches t c)
;;
