(** Core utilities for WebIDL bindings *)
open Browser_js_types

(** Function composition operator *)
val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

(** Identity function *)
val id : 'a -> 'a

(** Unsafe identity coercion *)
external very_unsafe_identity : _ -> 'a = "%identity"

(** JavaScript string constants *)
val string : Js.js_string Js.t

val boolean : Js.js_string Js.t
val number : Js.js_string Js.t
val constructor : Js.js_string Js.t
val object_ : Js.js_string Js.t
val function_ : Js.js_string Js.t

(** JavaScript number constants *)
val max_int : Js.number Js.t

val min_int : Js.number Js.t
val true_ : bool Js.t

(** Convert OCaml array to JavaScript any array with optional element converter *)
val array_to_any_array : ?elt_converter:('a -> 'b) -> 'a array -> Js.Unsafe.any array

(** Convert value to JavaScript any with optional converter *)
val to_any : ?elt_converter:('a -> 'b) -> 'a -> Js.Unsafe.any

(** Append arrays with second array as named parameter *)
val array_append : snd:'a array -> 'a array -> 'a array

(** Get class object from the global object for the specified class *)
val get_class_obj : global:'a Js.t -> string -> 'b

(** Get class object from the global object variant for the specified class *)
val get_class_obj_from_global
  :  global:
       [< `Animation_worklet of 'b Js.t
       | `Audio_worklet of 'c Js.t
       | `Dedicated_worker of 'd Js.t
       | `Layout_worklet of 'e Js.t
       | `Paint_worklet of 'f Js.t
       | `Service_worker of 'g Js.t
       | `Shadow_realm of 'h Js.t
       | `Shared_storage_worklet of 'i Js.t
       | `Shared_worker of 'j Js.t
       | `Unknown of 'k Js.t
       | `Window of 'l Js.t
       | `Undefined
       ]
  -> string
  -> 'a Js.optdef

(** JavaScript Array utilities *)
module Js_array : sig
  (** Utilities for any JavaScript arrays *)
  module Any : sig
    (** Remove and return first element from array *)
    val shift : Js.Unsafe.any_js_array -> 'a Js.t
  end

  (** Test if all elements in array satisfy predicate *)
  val every : 'b Js.t Js.js_array Js.t -> f:('b Js.t -> bool) -> bool

  (** Check if value is a JavaScript array *)
  val is_array : 'a Js.t -> bool

  (** Check if value is a uniform array with elements matching type checkers *)
  val is_uniform_array : 'a Js.t -> elt_type_checkers:('b Js.t -> bool) list -> bool

  (** Convert JavaScript array to OCaml array *)
  val to_array : 'a Js.js_array Js.t -> 'a array

  (** Convert JavaScript array to OCaml array with element mapping *)
  val to_array_map_elements : 'a Js.js_array Js.t -> f:('a -> 'b) -> 'b array

  (** Convert OCaml array to JavaScript array *)
  val of_array : 'a array -> 'a Js.js_array Js.t

  (** Convert OCaml array to JavaScript array with element mapping *)
  val of_array_map_elements : 'a array -> f:('a -> 'b) -> 'b Js.js_array Js.t
end

(** JavaScript Promise utilities *)
module Js_promise : sig
  (** Convert JavaScript promise to Promise_jsoo promise *)
  val to_promise_jsoo : 'a js_promise Js.t -> 'a Promise.t

  (** Convert Promise_jsoo promise to JavaScript promise *)
  val of_promise_jsoo : 'a Promise.t -> 'a js_promise Js.t
end
