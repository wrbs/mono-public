(* lib/webidl/bindings/high_level/core_utils.ml *)
open Gen_js_api
open Browser_js_types
include Constants

let ( >> ) (f : 'a -> 'b) (g : 'b -> 'c) (x : 'a) : 'c = g (f x)
let id (x : 'a) : 'a = x

external very_unsafe_identity : _ -> 'a = "%identity"

let get_class_obj ~global type_ =
  let constr = Js.Unsafe.get global (Js.string type_) in
  match Js.Optdef.test (Js.def constr) with
  | true -> constr
  | false ->
    failwith ("Global scope does not contain constructor for specified type " ^ type_)
;;

let get_class_obj_from_global ~global type_ =
  let type_ = Js.string type_ in
  match global with
  | `Animation_worklet global -> Js.Unsafe.get global type_
  | `Audio_worklet global -> Js.Unsafe.get global type_
  | `Dedicated_worker global -> Js.Unsafe.get global type_
  | `Layout_worklet global -> Js.Unsafe.get global type_
  | `Paint_worklet global -> Js.Unsafe.get global type_
  | `Service_worker global -> Js.Unsafe.get global type_
  | `Shadow_realm global -> Js.Unsafe.get global type_
  | `Shared_storage_worklet global -> Js.Unsafe.get global type_
  | `Shared_worker global -> Js.Unsafe.get global type_
  | `Window global -> Js.Unsafe.get global type_
  | `Unknown global -> Js.Unsafe.get global type_
  | `Undefined -> Js.Optdef.empty
;;

let array_to_any_array ?elt_converter (arr : 'a array) =
  match elt_converter with
  | Some elt_converter -> Array.map (elt_converter >> Js.Unsafe.inject) arr
  | None -> Array.map Js.Unsafe.inject arr
;;

let to_any ?elt_converter (x : 'a) =
  match elt_converter with
  | Some elt_converter -> Js.Unsafe.inject (elt_converter x)
  | None -> Js.Unsafe.inject x
;;

let array_append ~snd fst = Array.append fst snd

module Js_array = struct
  module Any = struct
    let shift (arr : Js.Unsafe.any_js_array) = Js.Unsafe.meth_call arr "shift" [||]
  end

  let every (arr : 'b Js.t Js.js_array Js.t) ~f =
    let callback =
      Js.wrap_meth_callback (fun () element _index _array -> f element |> Js.bool)
    in
    arr##every callback |> Js.to_bool
  ;;

  (** [Array] is an ECMAScript API so it should be available on every javascript
      environment. We pull it at startup in an attempt to avoid shadowing/reassignment *)
  let array_class : js_array_class Js.t = Js.Unsafe.js_expr {|globalThis.Array|}

  let is_array (x : 'a Js.t) = array_class##isArray x == true_

  let is_uniform_array (x : 'a Js.t) ~(elt_type_checkers : ('b Js.t -> bool) list) =
    match is_array x with
    | true ->
      let array_js : 'b Js.t Js.js_array Js.t = Js.Unsafe.coerce x in
      every array_js ~f:(fun x ->
        List.exists (fun check_type -> check_type x) elt_type_checkers)
    | false -> false
  ;;

  let to_array arr = Array.init arr##.length (fun i -> Js.Unsafe.get arr i)

  let to_array_map_elements arr ~f =
    Array.init arr##.length (fun i -> Js.Unsafe.get arr i |> f)
  ;;

  let of_array arr =
    let js_arr = new%js Js.array_length (Array.length arr) in
    Array.iteri (fun i element -> Js.Unsafe.set js_arr i element) arr;
    js_arr
  ;;

  let of_array_map_elements arr ~f =
    let set js_arr i element =
      let element = f element in
      Js.Unsafe.set js_arr i element
    in
    let js_arr = new%js Js.array_length (Array.length arr) in
    Array.iteri (fun i element -> set js_arr i element) arr;
    js_arr
  ;;
end

module Js_promise = struct
  let to_promise_jsoo (type a) (promise : a js_promise Js.t) : a Promise.t =
    let ojs = (Obj.magic : a js_promise Js.t -> Ojs.t) promise in
    Promise.t_of_js (Obj.magic : Ojs.t -> a) ojs
  ;;

  let of_promise_jsoo (type a) (promise : a Promise.t) : a js_promise Js.t =
    let ojs = Promise.t_to_js (Obj.magic : a -> Ojs.t) promise in
    (Obj.magic : Ojs.t -> a js_promise Js.t) ojs
  ;;
end
