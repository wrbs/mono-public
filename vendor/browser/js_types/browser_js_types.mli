module Js = Js_of_ocaml.Js

(** [unsafe] is used to mark some member of a class as unsafe. It can only be accessed by
    using [ppx_browser], which performs runtime checks to ensure that we are in the
    correct context.

    See [lib/browser/ppx/ppx_browser.mli] to learn more *)
type (+'a, 'b) unsafe

class type js_array_class = object
  method isArray : 'a Js.t -> bool Js.t Js.meth
end

val _Array : Js.js_string Js.t

class type js_object_class = object
  method hasOwn : 'a Js.t -> Js.js_string Js.t -> bool Js.t Js.meth
end

val _Object : Js.js_string Js.t

class type js_number_class = object
  method isInteger : 'a Js.t -> bool Js.t Js.meth
end

val _Number : Js.js_string Js.t

(* JavaScript Iterator interface *)
type ('k, 'v) entry

class type ['a] js_iterator_result = object
  method done_ : bool Js.t Js.readonly_prop
  method value : 'a Js.readonly_prop
end

class type ['a] js_iterator = object
  method next : 'a js_iterator_result Js.t Js.meth
  method toArray : 'a Js.js_array Js.t Js.meth
end

class type ['k, 'v] js_iterable = object
  method forEach : (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
  method keys : 'k js_iterator Js.t Js.meth
  method values : 'v js_iterator Js.t Js.meth
  method entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
end

class type ['k, 'v, 'context] js_iterable_unsafe = object
  method forEach_unsafe :
    ( (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
      , 'context )
      unsafe

  method keys_unsafe : ('k js_iterator Js.t Js.meth, 'context) unsafe
  method values_unsafe : ('v js_iterator Js.t Js.meth, 'context) unsafe
  method entries_unsafe : (('k, 'v) entry Js.t js_iterator Js.t Js.meth, 'context) unsafe
end

class type ['v] js_indexed_iterable = object
  inherit [Js.number_t, 'v] js_iterable
  method length : Js.number Js.t Js.readonly_prop
end

class type ['v, 'context] js_indexed_iterable_unsafe = object
  inherit [Js.number_t, 'v, 'context] js_iterable_unsafe
  method length_unsafe : (Js.number Js.t Js.readonly_prop, 'context) unsafe
end

class type ['k, 'v] js_readonly_maplike = object
  method size : Js.number Js.t Js.readonly_prop
  method forEach : (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
  method get : 'k -> 'v Js.optdef Js.meth
  method has : 'k -> bool Js.t Js.meth
  method keys : 'k js_iterator Js.t Js.meth
  method values : 'v js_iterator Js.t Js.meth
  method entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
end

class type ['k, 'v] js_maplike = object
  inherit ['k, 'v] js_readonly_maplike
  method clear : unit Js.meth
  method delete : 'k -> bool Js.t Js.meth
  method set : key:'k -> value:'v -> unit Js.meth
end

class type ['v] js_readonly_setlike = object
  method size : Js.number Js.t Js.readonly_prop
  method forEach : (unit, 'v -> 'v -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
  method has : 'v -> bool Js.t Js.meth
  method values : 'v js_iterator Js.t Js.meth
  method entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
end

class type ['v] js_setlike = object
  inherit ['v] js_readonly_setlike
  method add : 'v -> unit Js.meth
  method clear : unit Js.meth
  method delete : 'v -> bool Js.t Js.meth
end

type 'a frozen_array = 'a array
type 'value record
type untranslated
type 'a js_promise
type event_id

(** [instance_t] is used for [instanceof]. The ['a] represents the type of the instance of
    the class that is created whenever you call the constructor of this class

    It's primarily useful for the type conversion exposed in [Browser_utils] *)
type +'a instance_t

val to_event_id : (unit -> unit) -> event_id
val remove_event_listener : event_id -> unit
