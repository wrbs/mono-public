open Ppxlib
open Ast_helper
open Asttypes
open Parsetree

let ghost_loc loc = { loc with loc_ghost = true }

let ghosted_extension_loc ctxt =
  Expansion_context.Extension.extension_point_loc ctxt |> ghost_loc
;;

module Context = struct
  type t =
    | Secure
    | Cross_origin_isolated

  let of_string = function
    | "Cross_origin_isolated" -> Ok Cross_origin_isolated
    | "Secure" -> Ok Secure
    | other -> Error ("Unsupported context [" ^ other ^ "]")
  ;;

  let to_string = function
    | Cross_origin_isolated -> "Cross_origin_isolated"
    | Secure -> "Secure"
  ;;

  let of_lident_loc (string_loc : longident loc) =
    let lident_loc =
      match string_loc.txt with
      | Lident txt ->
        (match of_string txt with
         | Ok txt -> Some { string_loc with txt }
         | Error _ -> None)
      | _ -> None
    in
    match lident_loc with
    | Some lident_loc -> Ok lident_loc
    | None ->
      Error "Path argument to %browser must be one of [Secure], [Cross_origin_isolated]"
  ;;

  let to_string_loc (t_loc : t loc) =
    let txt = to_string t_loc.txt in
    { t_loc with txt }
  ;;
end

let create_input_output_types ~loc (context : Context.t loc) =
  let context_type =
    (* [context_type] is required so that we raise if the type of the context the user
       provided for the ppx is not the same as the context type of the current
       method/property *)
    { ptyp_desc =
        Ptyp_variant
          ( [ { prf_desc = Rtag (Context.to_string_loc context, true, [])
              ; prf_loc = loc
              ; prf_attributes = []
              }
            ]
          , Open
          , None )
    ; ptyp_loc = loc
    ; ptyp_loc_stack = []
    ; ptyp_attributes = []
    }
  in
  (* Hide all of the types from merlin to avoid the location collision errors from ppx
     rewriters *)
  ( [%type: ((a, [%t context_type]) Browser_js_types.unsafe[@merlin.hide])]
  , [%type: (a[@merlin.hide])] )
;;

(** [create_conv_fun] converts the imaginary "unsafe" type into its safe to use
    counterpart. It is type constrained so that we throw type errors if we are not
    checking for the appropriate context for this [unsafe] type *)
let create_conv_fun ~loc ~input_type ~output_type (input_obj : expression) =
  [%expr
    (fun (type a) (x : [%t input_type]) : [%t output_type] ->
      (Obj.magic x : [%t output_type]))
      [%e input_obj]]
;;

(** [convert_constructor] is a bit different due to [ppx_js] requiring that constructors
    are bound to an identifier prior to use. It's okay if the variable is shadowed later
    down the line, as we've already constructed the type we wanted to. *)
let convert_constructor ~loc ~context ~args (constr : longident_loc) =
  let loc = ghost_loc loc in
  let constr_loc = ghost_loc constr.loc in
  let constr =
    let loc = constr_loc in
    { constr with loc }
  in
  let input_type, output_type =
    let loc = constr_loc in
    create_input_output_types ~loc context
  in
  let constr_expr =
    let loc = constr_loc in
    { pexp_desc = Pexp_ident constr
    ; pexp_loc = loc
    ; pexp_loc_stack = [ loc ]
    ; pexp_attributes = []
    }
  in
  let conv_fn =
    let loc = constr_loc in
    create_conv_fun ~loc ~input_type ~output_type constr_expr
  in
  let new_js_expr = [%expr new%js __ppx_browser_constr_temp_name_for_type_conv] in
  let new_js_expr =
    match args with
    | None -> new_js_expr
    | Some args ->
      { pexp_desc = Pexp_apply (new_js_expr, args)
      ; pexp_loc = loc
      ; pexp_loc_stack = [ loc ]
      ; pexp_attributes = []
      }
  in
  [%expr
    let __ppx_browser_constr_temp_name_for_type_conv = [%e conv_fn] in
    [%e new_js_expr]]
;;

(** [convert_to_object] takes an object with [unsafe] method [meth] and converts it to an
    object with field [meth], where [meth] is now the safe version of the [unsafe] method *)
let convert_to_object ~(context : Context.t loc) obj (meth : expression) =
  let meth =
    match meth.pexp_desc with
    | Pexp_ident { txt = Lident txt; loc } -> { txt; loc = ghost_loc loc }
    | _ ->
      Location.raise_errorf
        ~loc:meth.pexp_loc
        "Trying to access ppx_js with something other than an identifier"
  in
  let obj_loc = ghost_loc obj.pexp_loc in
  let input_type, output_type = create_input_output_types ~loc:obj_loc context in
  let input_type =
    { ptyp_desc =
        Ptyp_object
          ( [ { pof_desc = Otag (meth, input_type)
              ; pof_loc = obj_loc
              ; pof_attributes = []
              }
            ]
          , Open )
    ; ptyp_loc = obj_loc
    ; ptyp_loc_stack = [ obj_loc ]
    ; ptyp_attributes = []
    }
  in
  let input_type =
    let loc = obj_loc in
    [%type: ([%t input_type] Js_of_ocaml.Js.t[@merlin.hide])]
  in
  let output_type =
    { ptyp_desc =
        Ptyp_object
          ( [ { pof_desc = Otag (meth, output_type)
              ; pof_loc = obj_loc
              ; pof_attributes = []
              }
            ]
          , Open )
    ; ptyp_loc = obj_loc
    ; ptyp_loc_stack = [ obj_loc ]
    ; ptyp_attributes = []
    }
  in
  let output_type =
    let loc = obj_loc in
    [%type: ([%t output_type] Js_of_ocaml.Js.t[@merlin.hide])]
  in
  create_conv_fun ~loc:obj_loc ~input_type ~output_type obj
;;

(** [transform] ensures that the ppx extensions to interact with unsafe values only exist
    within the [%browser.CONTEXT] expression. It converts directly to [ppx_js] expressions
    so that we don't have to re-implement the functionality that already exists *)
let transform ~(context : Context.t loc) =
  object (self)
    inherit Ast_traverse.map as super

    method! expression expr =
      let prev_default_loc = !default_loc in
      default_loc := expr.pexp_loc;
      let { pexp_attributes; _ } = expr in
      let new_expr =
        match expr with
        (* obj##!.var *)
        | [%expr [%e? obj] ##!. [%e? meth]] ->
          let obj = self#expression obj in
          let obj = convert_to_object ~context obj meth in
          let loc = expr.pexp_loc in
          self#expression [%expr [%e obj] ##. [%e meth]]
        (* obj##!.var := value *)
        | [%expr [%e? [%expr [%e? obj] ##!. [%e? meth]] as prop] := [%e? value]] ->
          let obj = self#expression obj in
          let value = self#expression value in
          let obj = convert_to_object ~context obj meth in
          let getter_expr =
            let loc = ghost_loc prop.pexp_loc in
            [%expr [%e obj] ##. [%e meth]]
          in
          let loc = ghost_loc expr.pexp_loc in
          self#expression [%expr [%e getter_expr] := [%e value]]
        (* obj##!(meth arg1 arg2) .. *)
        | [%expr [%e? obj] ##! [%e? { pexp_desc = Pexp_apply (meth, _); _ } as args]] ->
          let obj = self#expression obj in
          let obj = convert_to_object ~context obj meth in
          let loc = ghost_loc expr.pexp_loc in
          self#expression [%expr [%e obj] ## [%e args]]
        (* obj##!meth arg1 arg2 .. *)
        | { pexp_desc = Pexp_apply ([%expr [%e? obj] ##! [%e? meth]], args); _ } ->
          let obj = self#expression obj in
          let obj = convert_to_object ~context obj meth in
          let new_expr =
            let loc = ghost_loc expr.pexp_loc in
            { expr with pexp_desc = Pexp_apply ([%expr [%e obj] ## [%e meth]], args) }
          in
          self#expression new_expr
        (* obj##!meth *)
        | [%expr [%e? obj] ##! [%e? meth]] ->
          let obj = self#expression obj in
          let obj = convert_to_object ~context obj meth in
          let loc = ghost_loc expr.pexp_loc in
          self#expression [%expr [%e obj] ## [%e meth]]
        (* new%unsafe_js constr] *)
        | [%expr [%js_unsafe [%e? { pexp_desc = Pexp_new constr; pexp_loc; _ }]]] ->
          let new_expr = convert_constructor ~loc:pexp_loc ~args:None ~context constr in
          self#expression { new_expr with pexp_attributes }
        (* new%unsafe_js constr arg1 arg2 ..)] *)
        | { pexp_desc =
              Pexp_apply
                ( [%expr [%js_unsafe [%e? { pexp_desc = Pexp_new constr; pexp_loc; _ }]]]
                , args )
          ; _
          } ->
          let new_expr =
            convert_constructor ~loc:pexp_loc ~args:(Some args) ~context constr
          in
          self#expression { new_expr with pexp_attributes }
        | _ -> super#expression expr
      in
      default_loc := prev_default_loc;
      new_expr
  end
;;

let context_to_variant (context : Context.t loc) : expression =
  let loc = context.loc |> ghost_loc in
  let context = { context with loc } in
  { pexp_desc =
      Pexp_construct ({ context with txt = Lident (Context.to_string context.txt) }, None)
  ; pexp_loc = loc
  ; pexp_loc_stack = [ loc ]
  ; pexp_attributes = []
  }
;;

let extensions =
  [ Extension.V3.declare_with_path_arg
      "browser"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      (fun ~ctxt ~arg expr ->
        match arg with
        | None ->
          Location.raise_errorf
            ~loc:expr.pexp_loc
            "Path argument to %s must be one of [Secure], [Cross_origin_isolated]"
            "%browser"
        | Some arg ->
          (match Context.of_lident_loc arg with
           | Ok context ->
             let loc = ghosted_extension_loc ctxt in
             let new_expr = (transform ~context)#expression expr in
             (* While it is still technically possible for users to shadow the runtime
                library and bypass this check, doing so would be an obvious misuse of the
                library so we should not need to account for that *)
             [%expr
               match
                 Ppx_browser_runtime.Unsafe_context_checker.check_context
                   Ppx_browser_syntax.check_context
                   ~context:[%e context_to_variant context]
               with
               | true -> Ok [%e new_expr]
               | false -> Error Ppx_browser_runtime.Unsafe.Error.Not_in_correct_context]
           | _ ->
             let loc = arg.loc in
             Location.raise_errorf
               ~loc
               "Path argument to %s must be one of [Secure], [Cross_origin_isolated]"
               "%browser"))
  ]
;;

let () = Driver.register_transformation "browser" ~extensions
