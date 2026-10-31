open! Base
open! Ppxlib
open! Ast_builder.Default
module Attrs = Unstring_attrs

module Product_args = struct
  type t =
    | Constructor_no_arg of { constructor : string }
    | Tuple of
        { constructor : string option
        ; args : (string option * expression) list
        }
    | Record of
        { constructor : string option
        ; args : (string * expression) list
        }
end

module Variant_kind = struct
  type t =
    | No_arg of
        { tag : string
        ; format : Format_string.t
        }
    | Arg of
        { tag : string
        ; expr : expression
        }
    | Inherit of
        { type_ : core_type
        ; expr : expression
        }
end

module type S = sig
  type product_result

  val product
    :  loc:Location.t
    -> Product_args.t
    -> format:Format_string.t
    -> product_result

  val sum : loc:Location.t -> product_result list -> expression
  val variant_row : loc:Location.t -> Variant_kind.t -> product_result

  val expr_of_format
    :  loc:Location.t
    -> Format_string.t
    -> args:[ `Zero | `One ]
    -> expression

  val reads_chars_attr : bool
  val prefix : string
  val expected_type : loc:Location.t -> core_type -> core_type
  val custom_attribute : (core_type, expression) Attribute.t
end

let error_message_for_type = function
  | Some "option" ->
    {| Hint: option types require [@some "..."] and [@none "..."] attributes to specify the format for Some and None cases.|}
  | Some "list" ->
    {| Hint: list types require a [@sep "..."] attribute to specify the separator between elements.|}
  | Some "unit" ->
    {| Hint: unit types require a [@unit "..."] attribute to specify the expected literal.|}
  | _ ->
    {| Hint: some types require attributes; did you forget [@some "..."], [@none "..."], [@unit "..."], [@format "..."], or [@sep "..."]?|}
;;

let type_name_of_core_type (core_type : core_type) =
  match core_type.ptyp_desc with
  | Ptyp_constr ({ txt = Lident name; _ }, _) -> Some name
  | Ptyp_constr ({ txt = Ldot (_, name); _ }, _) -> Some name
  | _ -> None
;;

module Make (S : S) = struct
  let binding_name = function
    | "t" -> S.prefix
    | type_name -> S.prefix ^ "_" ^ type_name
  ;;

  let wrap_with_error_message ~loc ~core_type expr =
    let type_name = type_name_of_core_type core_type in
    let message = error_message_for_type type_name in
    [%expr
      ([%e expr]
       : [%t S.expected_type ~loc [%type: _]])
       [@error_message [%e estring ~loc message]]]
  ;;

  let of_type_inner ~type_params ~of_type ~loc (core_type : core_type) : expression =
    match Attribute.get S.custom_attribute core_type with
    | Some custom_expr -> custom_expr
    | None ->
      (match core_type.ptyp_desc with
       | Ptyp_constr (({ loc; _ } as longident), args) ->
         let name_suffix, chars_expr =
           (let%bind.Option () = if S.reads_chars_attr then Some () else None in
            let%map.Option chars_expr = Attribute.get Attrs.string_chars core_type in
            "_chars", Some chars_expr)
           |> Option.value ~default:("", None)
         in
         let base =
           unapplied_type_constr_conv
             longident
             ~f:(fun type_name -> binding_name type_name ^ name_suffix)
             ~loc
         in
         let labeled_args =
           [ (let%map.Option sep = Attribute.get Attrs.list_sep core_type in
              Labelled "sep", S.expr_of_format ~loc sep ~args:`Zero)
           ; (let%map.Option some = Attribute.get Attrs.option_some core_type in
              Labelled "some", S.expr_of_format ~loc some ~args:`One)
           ; (let%map.Option none = Attribute.get Attrs.option_none core_type in
              Labelled "none", S.expr_of_format ~loc none ~args:`Zero)
           ; (let%map.Option unit = Attribute.get Attrs.unit_format core_type in
              Labelled "unit", S.expr_of_format ~loc unit ~args:`Zero)
           ; (let%map.Option chars_expr in
              Labelled "chars", chars_expr)
           ]
           |> List.filter_map ~f:Fn.id
         in
         let base =
           let type_arg_exprs = List.map args ~f:(fun arg -> Nolabel, of_type ~loc arg) in
           let all_args = labeled_args @ type_arg_exprs in
           match all_args with
           | [] -> base
           | _ -> pexp_apply ~loc base all_args
         in
         (match Attribute.get Attrs.format_type core_type with
          | None -> base
          | Some format -> [%expr [%e S.expr_of_format ~loc format ~args:`One] [%e base]])
       | Ptyp_variant (rows, Closed, _) ->
         List.map rows ~f:(fun ({ prf_desc; prf_loc = loc; _ } as row) ->
           let (variant_kind : Variant_kind.t) =
             match prf_desc with
             | Rtag ({ txt = tag; loc }, accepts_no_argument, possible_types) ->
               (match accepts_no_argument, possible_types with
                | _, [] ->
                  assert accepts_no_argument;
                  let format =
                    match Attribute.get Attrs.format_rtag row with
                    | Some format -> format
                    | None ->
                      Location.raise_errorf ~loc "[@format] is required for each variant"
                  in
                  No_arg { tag; format }
                | false, [ arg ] -> Arg { tag; expr = of_type ~loc arg }
                | _ ->
                  Location.raise_errorf
                    ~loc:row.prf_loc
                    "ppx_unstring: polymorphic variants with more than one argument type \
                     (the `&` syntax) are not supported.")
             | Rinherit type_ -> Inherit { type_; expr = of_type ~loc type_ }
           in
           S.variant_row ~loc variant_kind)
         |> S.sum ~loc
       | Ptyp_var (name, _) ->
         (match Map.find type_params name with
          | Some param_name -> evar ~loc param_name
          | None -> Location.raise_errorf ~loc "Type variable '%s' not found" name)
       | Ptyp_tuple elements ->
         let format =
           match Attribute.get Attrs.format_type core_type with
           | Some format -> format
           | None -> Location.raise_errorf ~loc "[@format] is required for tuple types"
         in
         let args =
           List.map elements ~f:(fun (label, type_) -> label, of_type ~loc type_)
         in
         [ S.product ~loc (Tuple { constructor = None; args }) ~format ] |> S.sum ~loc
       | _ -> Location.raise_errorf ~loc "Unsupported type for [%%unstring: ...]")
  ;;

  let rec of_type ~type_params ~loc core_type =
    of_type_inner ~type_params ~of_type:(of_type ~type_params) ~loc core_type
    |> wrap_with_error_message ~loc ~core_type
  ;;

  let args_of_type (ctyp : core_type) =
    match ctyp.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "unit"; _ }, []) -> []
    | Ptyp_tuple elements -> elements
    | _ -> [ None, ctyp ]
  ;;

  let extension ~format ~type_ ~loc =
    let type_params = Map.empty (module String) in
    let args =
      args_of_type type_
      |> List.map ~f:(fun (label, arg) -> label, of_type ~type_params ~loc arg)
    in
    match args with
    | [] -> S.expr_of_format ~loc format ~args:`Zero
    | args ->
      [ S.product ~loc (Tuple { constructor = None; args }) ~format ] |> S.sum ~loc
  ;;

  let type_params_of_type_decl ~type_decl =
    List.filter_map type_decl.ptype_params ~f:(fun (core_type, _) ->
      match core_type.ptyp_desc with
      | Ptyp_var (name, _) -> Some (name, name)
      | _ -> None)
    |> Map.of_alist_exn (module String)
  ;;

  let wrap_with_type_params ~loc ~type_decl expr =
    let params =
      List.filter_map type_decl.ptype_params ~f:(fun (core_type, _) ->
        match core_type.ptyp_desc with
        | Ptyp_var (name, _) -> Some name
        | _ -> None)
    in
    match params with
    | [] -> expr
    | _ ->
      List.fold_right params ~init:expr ~f:(fun param_name body ->
        pexp_fun ~loc Nolabel None (pvar ~loc param_name) body)
  ;;

  let deriver ~type_decl ~loc =
    let type_params = type_params_of_type_decl ~type_decl in
    let expr =
      match type_decl.ptype_kind with
      | Ptype_record fields ->
        let format =
          match Attribute.get Attrs.format_type_decl type_decl with
          | Some format -> format
          | None -> Location.raise_errorf ~loc "Record types require [@@@@format \"...\"]"
        in
        let args =
          List.map fields ~f:(fun field ->
            field.pld_name.txt, of_type ~type_params ~loc field.pld_type)
        in
        [ S.product ~loc (Record { constructor = None; args }) ~format ] |> S.sum ~loc
      | Ptype_variant constructors ->
        List.map constructors ~f:(fun constructor ->
          let format =
            match Attribute.get Attrs.format constructor with
            | Some format -> format
            | None ->
              Location.raise_errorf
                ~loc:constructor.pcd_loc
                "Missing [@format \"...\"] attribute on constructor %s"
                constructor.pcd_name.txt
          in
          let constructor_name = constructor.pcd_name.txt in
          match constructor.pcd_args with
          | Pcstr_tuple [] ->
            S.product ~loc (Constructor_no_arg { constructor = constructor_name }) ~format
          | Pcstr_tuple types ->
            let args =
              List.map types ~f:(fun type_ ->
                None, of_type ~type_params ~loc type_.pca_type)
            in
            S.product ~loc (Tuple { constructor = Some constructor_name; args }) ~format
          | Pcstr_record fields ->
            let args =
              List.map fields ~f:(fun f ->
                f.pld_name.txt, of_type ~type_params ~loc f.pld_type)
            in
            S.product ~loc (Record { constructor = Some constructor_name; args }) ~format)
        |> S.sum ~loc
      | Ptype_abstract ->
        let manifest =
          match type_decl.ptype_manifest with
          | Some manifest -> manifest
          | None ->
            Location.raise_errorf
              ~loc
              "Abstract types without manifest cannot derive unstring"
        in
        of_type ~type_params ~loc manifest
      | Ptype_open -> Location.raise_errorf ~loc "Open types are not supported"
      | Ptype_record_unboxed_product _ ->
        Location.raise_errorf ~loc "Unboxed record types are not supported"
    in
    let expr = wrap_with_type_params ~type_decl ~loc expr in
    let name = binding_name type_decl.ptype_name.txt in
    [%stri let [%p pvar ~loc name] = [%e expr]]
  ;;

  let sig_item ~type_decl ~loc =
    let type_name = type_decl.ptype_name.txt in
    let params = List.map type_decl.ptype_params ~f:(fun (core_type, _) -> core_type) in
    let result_type = ptyp_constr ~loc { txt = Lident type_name; loc } params in
    let full_type =
      List.fold_right params ~init:(S.expected_type ~loc result_type) ~f:(fun param acc ->
        match param.ptyp_desc with
        | Ptyp_var (name, _) ->
          [%type: [%t S.expected_type ~loc (ptyp_var ~loc name)] -> [%t acc]]
        | _ -> acc)
    in
    psig_value
      ~loc
      (value_description
         ~loc
         ~name:{ txt = binding_name type_name; loc }
         ~type_:full_type
         ~prim:[])
  ;;
end
