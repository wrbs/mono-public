open! Base
open Ppxlib

let extension =
  Extension.V3.declare
    "sexp_digest"
    Extension.Context.expression
    Ast_pattern.(ptyp __)
    Ppx_sexp_digest_expander.expand
;;

let () = Driver.register_transformation "sexp_digest" ~extensions:[ extension ]
