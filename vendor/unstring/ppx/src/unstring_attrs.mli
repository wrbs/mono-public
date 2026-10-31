open! Ppxlib

(** Ppxlib attribute declarations for [ppx_unstring].

    These define the [@format], [@some], [@none], [@custom], [@sep], and [@chars]
    attributes recognized by the ppx on various AST contexts. *)

val format : (constructor_declaration, Format_string.t) Attribute.t
val format_rtag : (row_field, Format_string.t) Attribute.t
val format_type : (core_type, Format_string.t) Attribute.t
val unit_format : (core_type, Format_string.t) Attribute.t
val format_type_decl : (type_declaration, Format_string.t) Attribute.t
val option_some : (core_type, Format_string.t) Attribute.t
val option_none : (core_type, Format_string.t) Attribute.t
val custom_unstring_core_type : (core_type, expression) Attribute.t
val custom_restring_core_type : (core_type, expression) Attribute.t
val list_sep : (core_type, Format_string.t) Attribute.t
val string_chars : (core_type, expression) Attribute.t
