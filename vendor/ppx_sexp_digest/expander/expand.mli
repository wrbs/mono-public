open Ppxlib

(** Code-writing function called by the sexp_digest rewriter *)
val expand : ctxt:Expansion_context.Extension.t -> core_type -> expression
