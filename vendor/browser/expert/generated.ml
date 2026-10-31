open! Import

type global_t =
  [ `Window of Types.Globals.window Js.t
  | `Dedicated_worker of Types.Globals.dedicated_worker_global_scope Js.t
  | `Shared_worker of Types.Globals.shared_worker_global_scope Js.t
  | `Unknown of < > Js.t
  | `Undefined
  ]

module Global = struct
  type t = global_t

  let get_class_obj_from_global ~(global : global_t) class_name =
    let class_name = Js.string class_name in
    match global with
    | `Window global -> Js.Unsafe.get global class_name
    | `Dedicated_worker global -> Js.Unsafe.get global class_name
    | `Shared_worker global -> Js.Unsafe.get global class_name
    | `Unknown global -> Js.Unsafe.get global class_name
    | `Undefined -> Js.Optdef.empty
  ;;

  let is_exact_instance_of ~(type_ : string) ~global obj : bool =
    Private_browser_bindings_shared.Type_checking_utils.is_exact_instance_of
      ~type_
      ~get_class_obj_from_global
      ~global
      obj
  ;;

  let get () : t =
    let window =
      try Js.Unsafe.js_expr "globalThis.window" with
      | _ -> Js.undefined
    in
    let self =
      try Js.Unsafe.js_expr "globalThis.self" with
      | _ -> Js.undefined
    in
    let global_this =
      try Js.Unsafe.js_expr "globalThis.globalThis" with
      | _ -> Js.undefined
    in
    if Js.Optdef.case window return_false (fun window ->
         is_exact_instance_of ~type_:"Window" ~global:(`Window window) window)
    then
      (* We have to re-fetch the value to prevent type errors *)
      `Window (Js.Unsafe.js_expr "window")
    else if Js.Optdef.case self return_false (fun self ->
              is_exact_instance_of
                ~type_:"DedicatedWorkerGlobalScope"
                ~global:(`Dedicated_worker self)
                self)
    then
      (* We have to re-fetch the value to prevent type errors *)
      `Dedicated_worker (Js.Unsafe.js_expr "self")
    else if Js.Optdef.case self return_false (fun self ->
              is_exact_instance_of
                ~type_:"SharedWorkerGlobalScope"
                ~global:(`Shared_worker self)
                self)
    then
      (* We have to re-fetch the value to prevent type errors *)
      `Shared_worker (Js.Unsafe.js_expr "self")
    else if Js.Optdef.test global_this
    then `Unknown (Js.Unsafe.js_expr "globalThis")
    else `Undefined
  ;;

  let is_secure_context globals =
    match globals with
    | `Window global_scope -> Js.to_bool global_scope##.isSecureContext
    | `Dedicated_worker global_scope -> Js.to_bool global_scope##.isSecureContext
    | `Shared_worker global_scope -> Js.to_bool global_scope##.isSecureContext
    | `Unknown _ | `Undefined -> false
  ;;

  let is_cross_origin_isolated globals =
    match globals with
    | `Window global_scope -> Js.to_bool global_scope##.crossOriginIsolated
    | `Dedicated_worker global_scope -> Js.to_bool global_scope##.crossOriginIsolated
    | `Shared_worker global_scope -> Js.to_bool global_scope##.crossOriginIsolated
    | `Unknown _ | `Undefined -> false
  ;;

  let is_in_context : type a. context:a Context.t -> t -> bool =
    fun ~context globals ->
    match context with
    | Secure -> is_secure_context globals
    | Cross_origin_isolated -> is_cross_origin_isolated globals
  ;;

  let window () =
    match get () with
    | `Window global -> Some global
    | _ -> None
  ;;

  let window_exn () =
    match get () with
    | `Window global -> global
    | `Unknown global when am_running_test -> Js.Unsafe.coerce global
    | _ -> failwith "Global object is not [Window]"
  ;;

  let dedicated_worker () =
    match get () with
    | `Dedicated_worker global -> Some global
    | _ -> None
  ;;

  let dedicated_worker_exn () =
    match get () with
    | `Dedicated_worker global -> global
    | `Unknown global when am_running_test -> Js.Unsafe.coerce global
    | _ -> failwith "Global object is not [DedicatedWorkerGlobalScope]"
  ;;

  let shared_worker () =
    match get () with
    | `Shared_worker global -> Some global
    | _ -> None
  ;;

  let shared_worker_exn () =
    match get () with
    | `Shared_worker global -> global
    | `Unknown global when am_running_test -> Js.Unsafe.coerce global
    | _ -> failwith "Global object is not [SharedWorkerGlobalScope]"
  ;;
end
