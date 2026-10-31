open Browser_js_types

let is_typeof
  (x : 'a Js.t)
  (kind : [ `String | `Boolean | `Number | `Object_ | `Function | `Null | `Undefined ])
  : bool
  =
  let open Constants in
  match kind with
  | `String -> string == Js.typeof x
  | `Boolean -> boolean == Js.typeof x
  | `Number -> number == Js.typeof x
  | `Object_ -> object_ == Js.typeof x
  | `Function -> function_ == Js.typeof x
  | `Null -> Js.Opt.strict_equals (Js.Opt.return x) Js.null
  | `Undefined -> Js.Optdef.strict_equals Js.undefined (Js.Optdef.return x)
;;

let is_string (x : 'a Js.t) : bool = is_typeof x `String
let is_bool (x : 'a Js.t) : bool = is_typeof x `Boolean
let is_object (x : 'a Js.t) : bool = is_typeof x `Object_
let is_function (x : 'a Js.t) : bool = is_typeof x `Function
let is_null (x : 'a Js.t) : bool = is_typeof x `Null
let is_undefined (x : 'a Js.t) : bool = is_typeof x `Undefined

let is_type_or_null ~type_checkers (x : 'a Js.t) : bool =
  is_null x || List.exists (fun check_type -> check_type x) type_checkers
;;

let is_type_or_undefined ~type_checkers (x : 'a Js.t) : bool =
  is_undefined x || List.exists (fun check_type -> check_type x) type_checkers
;;

let is_float (x : 'a Js.t) : bool = is_typeof x `Number

let is_enum (x : 'a Js.t) ~(f : Js.js_string Js.t -> bool) =
  match is_string x with
  | true ->
    let string_js = Js.Unsafe.coerce x in
    f string_js
  | false -> false
;;

(** [Object] is an ECMAScript API so it should be available on every JS environment. We
    retrieve it here in order to avoid shadowing/reassignment *)
let object_class : js_object_class Js.t = Js.Unsafe.js_expr {|globalThis.Object|}

let is_dictionary ?(required_fields : string list = []) (x : 'a Js.t) =
  match is_object x with
  | true ->
    let hasOwn property = object_class##hasOwn x (Js.string property) |> Js.to_bool in
    List.for_all hasOwn required_fields
  | false -> false
;;

let check_all_object_values =
  Js.Unsafe.pure_js_expr
    {js| function (obj, check) {
               for (const key in obj) {
                 if (!check(obj[key])){
                   return false;
                 }
               }
               return true;
             } |js}
;;

(* [check] takes a ['b Js.t] as the element type is variable *)
let check_all_object_values : 'a 'b. 'a Js.t -> ('b Js.t -> bool) -> bool =
  fun (type a b) (x : a Js.t) (check_value_type : b Js.t -> bool) ->
  let check_value_type =
    Js.wrap_meth_callback (fun () element -> check_value_type element |> Js.bool)
  in
  Js.Unsafe.fun_call
    check_all_object_values
    [| Js.Unsafe.inject x; Js.Unsafe.inject check_value_type |]
  |> Js.to_bool
;;

let is_record (x : 'a Js.t) ~(value_type_checkers : ('b Js.t -> bool) list) =
  match is_object x with
  | false -> false
  | true -> List.exists (check_all_object_values x) value_type_checkers
;;

let constructor = Js.string "constructor"

let is_exact_instance_of
  ~get_class_obj_from_global
  ~(type_ : string)
  ~global
  (class_instance : 'a Js.t)
  : bool
  =
  let class_constr = get_class_obj_from_global ~global type_ in
  let class_instance_constr = Js.Unsafe.get class_instance constructor in
  (* If the class object exists on the global and the class instance has a constructor *)
  Js.Optdef.(test (return class_instance_constr) && test (return class_constr))
  (* Then, check if the constructors are equal *)
  && class_instance_constr == class_constr
;;
