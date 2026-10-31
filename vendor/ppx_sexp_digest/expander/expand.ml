open! Base
open Ppxlib

let unsupported_typ_error_extension ~loc ~typ =
  let unsupported =
    Ppxlib_jane.Constructor_name.of_core_type_desc
      (Ppxlib_jane.Shim.Core_type_desc.of_parsetree typ.ptyp_desc)
  in
  let string_msg =
    String.concat
      ~sep:" "
      [ "Unsupported sexp_digest payload:"; unsupported; Ppxlib.string_of_core_type typ ]
  in
  Ast_builder.Default.pexp_extension ~loc
  @@ Location.error_extensionf ~loc "%s" string_msg
;;

let expand ~ctxt typ =
  let { ptyp_desc = desc; ptyp_loc = loc; _ } = typ in
  let loc = { loc with loc_ghost = true } in
  match desc with
  | Ptyp_constr (_, _) | Ptyp_tuple _ | Ptyp_variant _ ->
    let sexp_of_t = Ppx_sexp_conv_expander.Sexp_of.core_type typ ~stackify:false in
    let sexp_grammar =
      Ppx_sexp_conv_expander.Sexp_grammar.core_type ~tags_of_doc_comments:false ~ctxt typ
    in
    [%expr
      Ppx_sexp_digest_lib.sexp_digest
        ~sexp_of_t:[%e sexp_of_t]
        ~sexp_grammar:[%e sexp_grammar]]
  | _ -> unsupported_typ_error_extension ~loc ~typ
;;
