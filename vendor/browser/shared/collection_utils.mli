(** Collection utilities for JavaScript indexed and keyed collections *)
open Browser_js_types

module Unsafe := Browser_manually_written_bindings.Unsafe
module Context := Browser_manually_written_bindings.Context

module type S_input = sig
  type global

  val get : unit -> global
  val is_in_context : 'a. context:'a Context.t -> global -> bool
end

(** Utilities for JavaScript indexed collections (collections with numeric indices and
    length) *)
module Js_indexed_collection (M : S_input) : sig
  val of_unsafe
    :  context:'context Context.t
    -> < length_unsafe : (Js.number Js.t Js.readonly_prop, 'context) unsafe
       ; values_unsafe : ('v js_iterator Js.t Js.meth, 'context) unsafe
       ; forEach_unsafe :
           ( (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
             , 'context )
             unsafe
       ; entries_unsafe : (('k, 'v) entry Js.t js_iterator Js.t Js.meth, 'context) unsafe
       ; .. >
    -> < length : Js.number Js.t Js.readonly_prop
       ; values : 'v js_iterator Js.t Js.meth
       ; forEach : (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
       ; entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
       ; .. >
         Unsafe.t

  (** Map over values in an indexed collection with a transformation function *)
  val map_values
    :  < length : Js.number Js.t Js.readonly_prop
       ; values : 'v js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> f:('v -> 'a)
    -> 'a array

  (** Get all values from an indexed collection as an array *)
  val get_values
    :  < length : Js.number Js.t Js.readonly_prop
       ; values : 'a js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> 'a array
end

(** Utilities for JavaScript keyed collections (collections with key-value pairs) *)
module Js_keyed_collection (M : S_input) : sig
  (** Only [keyed_iterable]s have an unsafe variant, so we are matching on all the
      properties that exist on it *)
  val of_unsafe
    :  context:'context Context.t
    -> < keys_unsafe : ('k js_iterator Js.t Js.meth, 'context) unsafe
       ; values_unsafe : ('v js_iterator Js.t Js.meth, 'context) unsafe
       ; forEach_unsafe :
           ( (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
             , 'context )
             unsafe
       ; entries_unsafe : (('k, 'v) entry Js.t js_iterator Js.t Js.meth, 'context) unsafe
       ; .. >
    -> < keys : 'k js_iterator Js.t Js.meth
       ; values : 'v js_iterator Js.t Js.meth
       ; forEach : (unit, 'v -> 'k -> 'this Js.t -> unit) Js.meth_callback -> unit Js.meth
       ; entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
       ; .. >
         Unsafe.t

  (** Get the length of a keyed collection by iterating through its values *)
  val get_length : < values : 'v js_iterator Js.t Js.meth ; .. > Js.t -> int

  (** Map over keys in a keyed collection with a transformation function *)
  val map_keys
    :  < keys : 'k js_iterator Js.t Js.meth ; values : 'v js_iterator Js.t Js.meth ; .. >
         Js.t
    -> f:('k -> 'a)
    -> 'a array

  (** Get all keys from a keyed collection as an array *)
  val get_keys
    :  < keys : 'a js_iterator Js.t Js.meth ; values : 'b js_iterator Js.t Js.meth ; .. >
         Js.t
    -> 'a array

  (** Map over values in a keyed collection with a transformation function *)
  val map_values
    :  < values : 'v js_iterator Js.t Js.meth ; .. > Js.t
    -> f:('v -> 'a)
    -> 'a array

  (** Get all values from a keyed collection as an array *)
  val get_values : < values : 'b js_iterator Js.t Js.meth ; .. > Js.t -> 'b array

  (** Get all key-value pairs from a keyed collection as an array of tuples *)
  val get_entries
    :  < entries : ('a, 'b) entry Js.t js_iterator Js.t Js.meth
       ; values : 'b js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> ('a * 'b) array

  (** Map over keys in key-value pairs from a keyed collection *)
  val map_entry_keys
    :  < entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
       ; values : 'v js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> f:('k -> 'a)
    -> ('a * 'v) array

  (** Map over values in key-value pairs from a keyed collection *)
  val map_entry_values
    :  < entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
       ; values : 'v js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> f:('v -> 'b)
    -> ('k * 'b) array

  (** Map over both keys and values in key-value pairs from a keyed collection *)
  val map_entries
    :  < entries : ('k, 'v) entry Js.t js_iterator Js.t Js.meth
       ; values : 'v js_iterator Js.t Js.meth
       ; .. >
         Js.t
    -> f_key:('k -> 'a)
    -> f_value:('v -> 'b)
    -> ('a * 'b) array
end
