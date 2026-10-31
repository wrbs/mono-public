(** Type checking utilities for JavaScript values *)

open Browser_js_types

(** Check if a JavaScript value is a string *)
val is_string : 'a Js.t -> bool

(** Check if a JavaScript value is a boolean *)
val is_bool : 'a Js.t -> bool

(** Check if a JavaScript value is an object *)
val is_object : 'a Js.t -> bool

(** Check if a JavaScript value is a function *)
val is_function : 'a Js.t -> bool

(** Check if a JavaScript value is null *)
val is_null : 'a Js.t -> bool

(** Check if a JavaScript value is undefined *)
val is_undefined : 'a Js.t -> bool

(** Check if a JavaScript value matches one of the given type checkers or is null *)
val is_type_or_null : type_checkers:('a Js.t -> bool) list -> 'a Js.t -> bool

(** Check if a JavaScript value matches one of the given type checkers or is undefined *)
val is_type_or_undefined : type_checkers:('a Js.t -> bool) list -> 'a Js.t -> bool

(** Check if a JavaScript value is a number (float) *)
val is_float : 'a Js.t -> bool

(** Check if a JavaScript value is an enum (string that passes the given predicate) *)
val is_enum : 'a Js.t -> f:(Js.js_string Js.t -> bool) -> bool

(** Check if a JavaScript value is a dictionary with optional required fields *)
val is_dictionary : ?required_fields:string list -> 'a Js.t -> bool

(** Check if a JavaScript value is a record with values matching the given type checkers *)
val is_record : 'a Js.t -> value_type_checkers:('b Js.t -> bool) list -> bool

(** Returns [true] if ['a Js.t] is an exact instance of [type_], meaning it does not
    return [true] if ['a Js.t] is a subclass of [type_] *)
val is_exact_instance_of
  : 'global 'constr 'a.
  get_class_obj_from_global:(global:'global -> string -> 'constr Js.constr Js.optdef)
  -> type_:string
  -> global:'global
  -> 'a Js.t
  -> bool

(** Checks ['a Js.t] using [typeof] *)
val is_typeof
  :  'a Js.t
  -> [ `String | `Boolean | `Number | `Object_ | `Function | `Null | `Undefined ]
  -> bool
