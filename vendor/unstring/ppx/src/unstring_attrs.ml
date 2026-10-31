open! Ppxlib

let format_string_pattern =
  Ast_pattern.(pstr (pstr_eval (pexp_constant (pconst_string __ __ __)) nil ^:: nil))
;;

let format =
  Attribute.declare
    "unstring.format"
    Attribute.Context.constructor_declaration
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let format_rtag =
  Attribute.declare
    "unstring.format"
    Attribute.Context.rtag
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let format_type =
  Attribute.declare
    "unstring.format"
    Attribute.Context.core_type
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let unit_format =
  Attribute.declare
    "unstring.unit"
    Attribute.Context.core_type
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let format_type_decl =
  Attribute.declare
    "unstring.format"
    Attribute.Context.type_declaration
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let option_some =
  Attribute.declare
    "unstring.some"
    Attribute.Context.core_type
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let option_none =
  Attribute.declare
    "unstring.none"
    Attribute.Context.core_type
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let custom_unstring_core_type =
  Attribute.declare
    "unstring.custom"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
;;

let custom_restring_core_type =
  Attribute.declare
    "unstring.custom_restring"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
;;

let list_sep =
  Attribute.declare
    "unstring.sep"
    Attribute.Context.core_type
    format_string_pattern
    (fun text loc _delimiter -> { Format_string.text; loc })
;;

let string_chars =
  Attribute.declare
    "unstring.chars"
    Attribute.Context.core_type
    Ast_pattern.(single_expr_payload __)
    (fun x -> x)
;;
