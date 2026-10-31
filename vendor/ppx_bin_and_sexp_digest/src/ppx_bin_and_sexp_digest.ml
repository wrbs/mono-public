open Ppxlib

let expand ~ctxt typ =
  let loc = { typ.ptyp_loc with loc_ghost = true } in
  let sexp_digest = Ppx_sexp_digest_expander.expand ~ctxt typ in
  let bin_digest = Bin_shape_expand.digest_extension ~loc ~hide_loc:false typ in
  [%expr [%e sexp_digest] ^ [%e bin_digest] |> Md5_lib.string |> Md5_lib.to_hex]
;;

let extension =
  Extension.V3.declare
    "bin_and_sexp_digest"
    Extension.Context.expression
    Ast_pattern.(ptyp __)
    expand
;;

let () = Driver.register_transformation "bin_and_sexp_digest" ~extensions:[ extension ]
