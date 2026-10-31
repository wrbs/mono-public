module Js = Js_of_ocaml.Js

type 'a t = 'a Browser_js_types.record Js.t

(** [Object] is an ECMAScript API so it should be available on every JS environment. We
    retrieve it here in order to avoid shadowing/reassignment *)
let object_constructor : 'a t Js.constr = Js.Unsafe.js_expr {|globalThis.Object|}

let create () =
  (* The unsafe coercion is required because otherwise the ['a] doesn't line up with the
     output type :( *)
  new%js object_constructor |> Js.Unsafe.coerce
;;

let get t key = Js.Optdef.to_option Js.(def (Unsafe.get t (string key)))
let get' t key = Js.Optdef.to_option Js.(def (Unsafe.get t key))
let set t key data = Js.Unsafe.set t (Js.string key) data
let set' t key data = Js.Unsafe.set t key data
let delete t data = Js.Unsafe.delete t data
