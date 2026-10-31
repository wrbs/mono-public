module Js = Js_of_ocaml.Js

(** Type representing a JavaScript number treated as a 64-bit integer *)
type t = private Js.number_t

(** Convert a float to a JavaScript number if it represents an integer *)
val of_float : float -> t option

(** Convert a float to a JavaScript number, raising an exception if it's not an integer *)
val of_float_exn : float -> t

(** Convert a JavaScript number to a float *)
val to_float : t -> float

(** Convert a JavaScript number to t if it represents a 64-bit integer *)
val of_js_number : Js.number_t -> t option

(** Convert a JavaScript number to t, raising an exception if it doesn't represent a
    64-bit integer *)
val of_js_number_exn : Js.number_t -> t

(** Convert t to a JavaScript number *)
val to_js_number : t -> Js.number_t

(** Check if a JavaScript value is a 64-bit integer *)
val is_int64 : 'a Js.t -> bool
