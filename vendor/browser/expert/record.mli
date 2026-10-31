module Js = Js_of_ocaml.Js

type 'a t = 'a Browser_js_types.record Js.t

val create : unit -> 'a t
val get : 'a t -> string -> 'a option
val get' : 'a t -> Js.js_string Js.t -> 'a option
val set : 'a t -> string -> 'a -> unit
val set' : 'a t -> Js.js_string Js.t -> 'a -> unit
val delete : 'a t -> 'a -> unit
