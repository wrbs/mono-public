(* ppx_unstring.ml *)

open Base
open! Ppxlib
open Ast_builder.Default

module Unstring_impl : Structure.S with type product_result = expression = struct
  type product_result = expression

  let product ~loc (product_args : Structure.Product_args.t) ~format =
    match product_args with
    | Constructor_no_arg { constructor } ->
      let base = Format_string.build_unstring format ~loc ~fields:None in
      [%expr
        Unstring.map [%e base] ~f:(fun () ->
          [%e pexp_construct ~loc { loc; txt = Lident constructor } None])]
    | Tuple { constructor; args = parsers } ->
      let combine args =
        let inner = Ppxlib_jane.Ast_builder.Default.pexp_tuple ~loc args in
        match constructor with
        | None -> inner
        | Some c -> pexp_construct ~loc { loc; txt = Lident c } (Some inner)
      in
      Format_string.build_unstring format ~loc ~fields:(Many { parsers; combine })
    | Record { constructor; args } ->
      let parsers = List.map args ~f:(fun (label, expr) -> Some label, expr) in
      let combine args =
        let fields =
          List.map args ~f:(fun (label, expr) ->
            let label =
              Option.value_exn
                label
                ~message:"ppx_unstring bug: record member doesn't have a label"
            in
            { loc = expr.pexp_loc; txt = Lident label }, expr)
        in
        let record = pexp_record ~loc fields None in
        match constructor with
        | None -> record
        | Some c -> pexp_construct ~loc { loc; txt = Lident c } (Some record)
      in
      Format_string.build_unstring format ~loc ~fields:(Many { parsers; combine })
  ;;

  let sum ~loc = function
    | [ parser ] -> parser
    | parsers -> [%expr Unstring.choice [%e elist ~loc parsers]]
  ;;

  let variant_row ~loc : Structure.Variant_kind.t -> _ = function
    | No_arg { tag; format } ->
      let base = Format_string.build_unstring format ~loc ~fields:None in
      [%expr Unstring.map [%e base] ~f:(fun () -> [%e pexp_variant ~loc tag None])]
    | Arg { tag; expr } ->
      [%expr
        Unstring.map [%e expr] ~f:(fun x -> [%e pexp_variant ~loc tag (Some [%expr x])])]
    | Inherit { expr; _ } -> [%expr Unstring.map [%e expr] ~f:(fun x -> (x :> _))]
  ;;

  let expr_of_format ~loc format ~args =
    match args with
    | `Zero -> Format_string.build_unstring format ~loc ~fields:None
    | `One ->
      let parser = [%expr __inner] in
      let f = Format_string.build_unstring format ~loc ~fields:(Singleton { parser }) in
      [%expr fun __inner -> [%e f]]
  ;;

  let reads_chars_attr = true
  let prefix = "unstring"
  let expected_type ~loc param = [%type: [%t param] Unstring.t]
  let custom_attribute = Unstring_attrs.custom_unstring_core_type
end

module Restring_impl : Structure.S with type product_result = pattern * expression =
struct
  type product_result = pattern * expression

  let product ~loc (product_args : Structure.Product_args.t) ~format =
    match product_args with
    | Constructor_no_arg { constructor } ->
      let pattern = ppat_construct ~loc { loc; txt = Lident constructor } None in
      pattern, Format_string.build_string format ~loc ~hole_string_exprs:[]
    | Tuple { constructor; args = restrings } ->
      let patterns, exprs =
        List.mapi restrings ~f:(fun i (label, restring) ->
          let var_name = "__field_" ^ Int.to_string i in
          ( (label, pvar ~loc var_name)
          , (label, [%expr [%e restring] [%e evar ~loc var_name]]) ))
        |> List.unzip
      in
      let tuple_pat = Ppxlib_jane.Ast_builder.Default.ppat_tuple ~loc patterns Closed in
      let pattern =
        match constructor with
        | None -> tuple_pat
        | Some c -> ppat_construct ~loc { loc; txt = Lident c } (Some tuple_pat)
      in
      pattern, Format_string.build_string format ~loc ~hole_string_exprs:exprs
    | Record { constructor; args } ->
      let patterns, exprs =
        List.mapi args ~f:(fun i (label, restring) ->
          let var_name = "__field_" ^ Int.to_string i in
          ( ({ loc; txt = Lident label }, pvar ~loc var_name)
          , (Some label, [%expr [%e restring] [%e evar ~loc var_name]]) ))
        |> List.unzip
      in
      let record_pat = ppat_record ~loc patterns Closed in
      let pattern =
        match constructor with
        | None -> record_pat
        | Some c -> ppat_construct ~loc { loc; txt = Lident c } (Some record_pat)
      in
      pattern, Format_string.build_string format ~loc ~hole_string_exprs:exprs
  ;;

  let sum ~loc restrings =
    List.map restrings ~f:(fun (lhs, rhs) -> case ~lhs ~guard:None ~rhs)
    |> pexp_function ~loc
  ;;

  let variant_row ~loc : Structure.Variant_kind.t -> _ = function
    | No_arg { tag; format } ->
      ( ppat_variant ~loc tag None
      , Format_string.build_string format ~loc ~hole_string_exprs:[] )
    | Arg { tag; expr } -> ppat_variant ~loc tag (Some [%pat? __x]), [%expr [%e expr] __x]
    | Inherit { type_; expr } ->
      let type_longident =
        match type_.ptyp_desc with
        | Ptyp_constr (lid, _) -> lid
        | _ ->
          Location.raise_errorf
            ~loc
            "ppx_unstring: inherited polymorphic variant must be a named type"
      in
      ( ppat_alias ~loc (ppat_type ~loc type_longident) (Located.mk ~loc "__x")
      , [%expr [%e expr] __x] )
  ;;

  let expr_of_format ~loc format ~args =
    match args with
    | `Zero -> Format_string.build_string format ~loc ~hole_string_exprs:[]
    | `One ->
      let f =
        Format_string.build_string
          format
          ~loc
          ~hole_string_exprs:[ None, [%expr __f __x] ]
      in
      [%expr
        fun __f ->
          ();
          fun __x -> [%e f]]
  ;;

  let reads_chars_attr = false
  let prefix = "restring"
  let expected_type ~loc param = [%type: [%t param] -> string]
  let custom_attribute = Unstring_attrs.custom_restring_core_type
end

module Unstring = Structure.Make (Unstring_impl)
module Restring = Structure.Make (Restring_impl)

(* Pattern that matches either a type payload or an expression payload *)
let unstring_payload =
  let open Ast_pattern in
  map (ptyp __) ~f:(fun f ctyp -> f (`Type ctyp))
  ||| map (pstr (pstr_eval __ nil ^:: nil)) ~f:(fun f expr -> f (`Expr expr))
;;

let unstring_unified =
  Extension.declare
    "ppx_unstring.unstring"
    Extension.Context.expression
    unstring_payload
    (fun ~loc ~path:_ payload ->
       match payload with
       (* [%unstring: type] - type-directed *)
       | `Type ctyp ->
         Unstring.of_type ~type_params:(Map.empty (module String)) ~loc ctyp
         |> Merlin_helpers.hide_expression
       (* [%unstring ("format" : type)] - unified format with type constraint *)
       | `Expr
           { pexp_desc =
               Pexp_constraint
                 ( { pexp_desc = Pexp_constant (Pconst_string (text, string_loc, _)); _ }
                 , Some type_
                 , _ )
           ; _
           } ->
         let format = { Format_string.text; loc = string_loc } in
         Unstring.extension ~loc ~format ~type_ |> Merlin_helpers.hide_expression
       (* [%unstring "format"] - just a literal string, treat as unit parser *)
       | `Expr { pexp_desc = Pexp_constant (Pconst_string (text, string_loc, _)); _ } ->
         let format = { Format_string.text; loc = string_loc } in
         Unstring.extension ~loc ~format ~type_:[%type: unit]
         |> Merlin_helpers.hide_expression
       | `Expr _ ->
         Location.raise_errorf
           ~loc
           "Expected [%%unstring: type] or [%%unstring (\"format\" : type)]")
;;

let unstring_with_restring =
  Extension.declare
    "ppx_unstring.unstring.with_restring"
    Extension.Context.expression
    unstring_payload
    (fun ~loc ~path:_ payload ->
       let unstring_expr, restring_expr =
         match payload with
         (* [%unstring.with_restring: type] - type-directed *)
         | `Type type_ ->
           let type_params = Map.empty (module String) in
           ( Unstring.of_type ~type_params ~loc type_
           , Restring.of_type ~type_params ~loc type_ )
         (* [%unstring.with_restring ("format" : type)] - format with type constraint *)
         | `Expr
             { pexp_desc =
                 Pexp_constraint
                   ( { pexp_desc = Pexp_constant (Pconst_string (text, string_loc, _)); _ }
                   , Some type_
                   , _ )
             ; _
             } ->
           let format = { Format_string.text; loc = string_loc } in
           Unstring.extension ~loc ~format ~type_, Restring.extension ~loc ~format ~type_
         (* [%unstring.with_restring "format"] - literal string, unit *)
         | `Expr { pexp_desc = Pexp_constant (Pconst_string (text, string_loc, _)); _ } ->
           let format = { Format_string.text; loc = string_loc } in
           ( Unstring.extension ~loc ~format ~type_:[%type: unit]
           , Restring.extension ~loc ~format ~type_:[%type: unit] )
         | `Expr _ ->
           Location.raise_errorf
             ~loc
             "Expected [%%unstring.with_restring: type] or [%%unstring.with_restring \
              (\"format\" : type)]"
       in
       [%expr [%e unstring_expr], [%e restring_expr]] |> Merlin_helpers.hide_expression)
;;

let () =
  Ppxlib.Driver.register_transformation
    "ppx_unstring"
    ~extensions:[ unstring_unified; unstring_with_restring ]
;;

(* ============================================================ *)
(* Deriving: [@@deriving unstring] for records and variants *)
(* ============================================================ *)

module Deriver = struct
  let of_string_name_of_type_name type_name =
    if String.equal type_name "t" then "of_string" else type_name ^ "_of_string"
  ;;

  module On_ambiguity = struct
    type t =
      | Raise
      | Raise_opaque
      | Pick_first

    let of_expression_exn (expr : expression) =
      match expr.pexp_desc with
      | Pexp_variant ("Raise", None) -> Raise
      | Pexp_variant ("Pick_first", None) -> Pick_first
      | Pexp_variant ("Raise_opaque", None) -> Raise_opaque
      | _ ->
        Location.raise_errorf
          ~loc:expr.pexp_loc
          "Expected `Raise, `Pick_first, or `Raise_opaque"
    ;;
  end

  let generate_of_string_binding ~loc ~type_decl ~on_ambiguity =
    (match type_decl.ptype_params with
     | _ :: _ ->
       Location.raise_errorf ~loc "~of_string is not supported for parameterized types"
     | [] -> ());
    let type_name = type_decl.ptype_name.txt in
    let binding_name = of_string_name_of_type_name type_name in
    let on_ambiguity_expr =
      match on_ambiguity with
      | On_ambiguity.Raise ->
        let result_type = ptyp_constr ~loc { txt = Lident type_name; loc } [] in
        let sexp_of_expr =
          pexp_extension ~loc ({ txt = "sexp_of"; loc }, PTyp result_type)
        in
        [%expr `Raise [%e sexp_of_expr]]
      | Raise_opaque -> [%expr `Raise (fun _ -> Sexp.Atom "<opaque>")]
      | Pick_first -> [%expr `Pick_first]
    in
    let of_string_expr =
      [%expr
        Staged.unstage
          (Unstring.make_of_string
             [%e evar ~loc (Unstring.binding_name type_name)]
             ~compile_when:`Lazily
             ~on_ambiguity:[%e on_ambiguity_expr])]
    in
    pstr_value
      ~loc
      Nonrecursive
      [ value_binding ~loc ~pat:(pvar ~loc binding_name) ~expr:of_string_expr ]
  ;;

  let generate_unstring ~loc ~type_decl ~of_string ~on_ambiguity ~restring =
    let unstring_items = [ Unstring.deriver ~type_decl ~loc ] in
    let of_string_items =
      match of_string, on_ambiguity with
      | false, None -> []
      | false, Some _ -> Location.raise_errorf ~loc "~on_ambiguity requires ~of_string"
      | true, None -> Location.raise_errorf ~loc "~of_string requires ~on_ambiguity"
      | true, Some on_ambiguity_expr ->
        let on_ambiguity = On_ambiguity.of_expression_exn on_ambiguity_expr in
        [ generate_of_string_binding ~loc ~type_decl ~on_ambiguity ]
    in
    let restring_items =
      match restring with
      | false -> []
      | true -> [ Restring.deriver ~type_decl ~loc ]
    in
    unstring_items @ of_string_items @ restring_items
  ;;

  let generator =
    Deriving.Generator.V2.make
      Deriving.Args.(
        empty +> flag "of_string" +> arg "on_ambiguity" __ +> flag "restring")
      (fun ~ctxt (_, type_decls) of_string on_ambiguity restring ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match type_decls with
        | [ type_decl ] ->
          generate_unstring ~loc ~type_decl ~of_string ~on_ambiguity ~restring
        | _ -> Location.raise_errorf ~loc "unstring deriving expects exactly one type")
  ;;
end

module Signature = struct
  let result_type_of_type_decl ~loc type_decl =
    let type_name = type_decl.ptype_name.txt in
    let params = List.map type_decl.ptype_params ~f:(fun (core_type, _) -> core_type) in
    match params with
    | [] -> ptyp_constr ~loc { txt = Lident type_name; loc } []
    | _ -> ptyp_constr ~loc { txt = Lident type_name; loc } params
  ;;

  let generate_of_string_sig_item ~loc ~type_decl =
    let binding_name = Deriver.of_string_name_of_type_name type_decl.ptype_name.txt in
    let result_type = result_type_of_type_decl ~loc type_decl in
    let staged_fn_type = [%type: string -> [%t result_type]] in
    psig_value
      ~loc
      (value_description
         ~loc
         ~name:{ txt = binding_name; loc }
         ~type_:staged_fn_type
         ~prim:[])
  ;;

  let generate_sig ~loc ~type_decl ~of_string ~restring =
    let unstring_item = Unstring.sig_item ~loc ~type_decl in
    let of_string_items =
      match of_string with
      | false -> []
      | true -> [ generate_of_string_sig_item ~loc ~type_decl ]
    in
    let restring_items =
      match restring with
      | false -> []
      | true -> [ Restring.sig_item ~loc ~type_decl ]
    in
    [ unstring_item ] @ of_string_items @ restring_items
  ;;

  let generator =
    Deriving.Generator.V2.make
      Deriving.Args.(empty +> flag "of_string" +> flag "restring")
      (fun ~ctxt (_, type_decls) of_string restring ->
        let loc = Expansion_context.Deriver.derived_item_loc ctxt in
        match type_decls with
        | [ type_decl ] -> generate_sig ~loc ~type_decl ~of_string ~restring
        | _ -> Location.raise_errorf ~loc "unstring deriving expects exactly one type")
  ;;
end

let () =
  Deriving.add
    "unstring"
    ~str_type_decl:Deriver.generator
    ~sig_type_decl:Signature.generator
  |> Deriving.ignore
;;
