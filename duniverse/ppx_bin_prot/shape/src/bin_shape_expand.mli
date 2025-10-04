open Ppxlib
open Deriving

val str_gen : (structure, rec_flag * type_declaration list) Generator.t
val sig_gen : (signature_item list, rec_flag * type_declaration list) Generator.t
val shape_extension : loc:Location.t -> hide_loc:bool -> core_type -> expression
val digest_extension : loc:Location.t -> hide_loc:bool -> core_type -> expression
