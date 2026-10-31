open! Import
module Js = Js_of_ocaml.Js
module Types = Browser_types
module Record = Record
module Int64 = Int64
module Context = Context
module Unsafe = Browser_manually_written_bindings.Unsafe
include Generated

let is_typeof = Private_browser_bindings_shared.Type_checking_utils.is_typeof

let is_nullish (x : 'a Js.t) : bool =
  let tester = Js.Unsafe.js_expr {|((x) => x == null)|} in
  Js.Unsafe.fun_call tester [| Js.Unsafe.inject x |] |> Js.to_bool
;;

(** [Number] is an ECMAScript API so it should be available on every javascript
    environment. We pull it at startup in an attempt to avoid shadowing/reassignment *)
let number_class : Browser_js_types.js_number_class Js.t =
  Js.Unsafe.js_expr {|globalThis.Number|}
;;

let is_int64 = Int64.is_int64

let is_int (x : 'a Js.t) : bool =
  match number_class##isInteger x == Constants.Bool.true_ with
  | true ->
    let int_js = Js.Unsafe.coerce x in
    Js.math##min Constants.Number.max_int int_js
    == Js.math##max Constants.Number.min_int int_js
  | false -> false
;;

let coerce : type a. to_:a Browser_js_types.instance_t -> _ Js.t -> a Js.t option =
  fun ~to_ input_object ->
  match
    (* This is a guard clause for if the global scope for some reason doesn't actually
       have the class object assigned to it. E.g. JSDom. *)
    is_nullish
      (* We have to do an [Obj.magic] here because [is_typeof] accepts a [Js.t]. We can't
         assign this to a variable and use it again in [Js.instanceof] because
         [Js.instanceof] accepts a [Js.constr] and not a [Js.t] *)
      (Obj.magic to_)
  with
  | true ->
    (* If the [instance_t] is null we can't coerce *)
    None
  | false ->
    (match
       Js.instanceof
         input_object
         (* This is unsafe but we need to do this since [instanceof] expects the input to
            be a [Js.constr]. These values should only be created by [Browser_expert], so
            we should be able to safely coerce here *)
         (Obj.magic to_)
     with
     | true -> Some (Js.Unsafe.coerce input_object)
     | false -> None)
;;

module For_open = struct
  module Ppx_browser_syntax = struct
    let check_context =
      Browser_manually_written_bindings.Unsafe_context_checker.make
        (module struct
          type global = Generated.Global.t

          let is_in_context = Generated.Global.is_in_context
          let get_global = Global.get
        end)
    ;;
  end
end
