module Js = Js_of_ocaml.Js

type t = Js.number_t

let of_float t =
  match Float.is_integer t with
  | true -> Some (Js.float t)
  | false -> None
;;

let of_float_exn t =
  match Float.is_integer t with
  | true -> Js.float t
  | false -> failwith ("float specified is not an integer: " ^ Float.to_string t)
;;

let to_float t = Js.to_float t

(** [Number] is an ECMAScript API so it should be available on every javascript
    environment. We pull it at startup in an attempt to avoid shadowing/reassignment *)
let number_class : Browser_js_types.js_number_class Js.t =
  Js.Unsafe.js_expr {|globalThis.Number|}
;;

let is_int64 (x : 'a Js.t) : bool = number_class##isInteger x |> Js.to_bool

let of_js_number t =
  match is_int64 t with
  | true -> Some t
  | false -> None
;;

let of_js_number_exn t =
  match is_int64 t with
  | true -> t
  | false ->
    failwith ("float specified is not an integer: " ^ Float.to_string (Js.to_float t))
;;

let to_js_number t = t
