(** This library contains utility functions that are compatible with [Browser_types]. They
    are already included in [Browser] *)

module Js = Js_of_ocaml.Js
module Types = Browser_types
module Record = Record
module Int64 = Int64
module Context = Browser_manually_written_bindings.Context
module Unsafe = Browser_manually_written_bindings.Unsafe
include module type of Generated

(** Check if a JavaScript value is a 64-bit integer *)
val is_int64 : 'a Js.t -> bool

(** Check if a JavaScript value is an OCaml-compatible integer *)
val is_int : 'a Js.t -> bool

(** Checks ['a Js.t] using [typeof] *)
val is_typeof
  :  'a Js.t
  -> [ `String | `Boolean | `Number | `Object_ | `Function | `Null | `Undefined ]
  -> bool

(** Returns [true] if the value is either [null] or [undefined] *)
val is_nullish : 'a Js.t -> bool

(** [coerce] accepts a ['a instance_t] and returns a ['a Js.t] if ['b Js.t] is
    [instanceof] [to_] *)
val coerce : 'a 'b. to_:'a Browser_js_types.instance_t -> 'b Js.t -> 'a Js.t option

module For_open : sig
  module Ppx_browser_syntax : sig
    val check_context : Browser_manually_written_bindings.Unsafe_context_checker.t
  end
end
