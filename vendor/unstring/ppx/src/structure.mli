open! Base
open! Ppxlib

(** Shared skeleton for ppx_unstring derivers.

    Both [unstring] (parsing) and [restring] (printing) traverse OCaml type structure in
    the same way — the difference is what code they emit at each node. This module factors
    out the shared traversal: an implementation of {!S} supplies the code-generation
    strategy, and {!Make} produces the full deriver from it. *)

(** The shape of a product type's arguments (constructor payload, tuple, or record), as
    seen by the code generator. *)
module Product_args : sig
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

(** A single row of a polymorphic variant, classified by its argument structure. *)
module Variant_kind : sig
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

(** Code-generation strategy for a single direction (parsing or printing).

    Implementations produce AST fragments for each kind of type structure node. The
    functor {!Make} calls these to walk the full type and assemble the result. *)
module type S = sig
  (** The intermediate result of processing a product (constructor / tuple / record). *)
  type product_result

  (** Generate code for a product type (tuple, record, or nullary constructor) with the
      given format string. *)
  val product
    :  loc:Location.t
    -> Product_args.t
    -> format:Format_string.t
    -> product_result

  (** Combine multiple [product_result]s into a single expression representing a choice
      (variant / sum type). *)
  val sum : loc:Location.t -> product_result list -> expression

  (** Generate code for a single polymorphic variant row. *)
  val variant_row : loc:Location.t -> Variant_kind.t -> product_result

  (** Turn a format string into an expression, used for attribute arguments like [@sep],
      [@some], [@none], [@unit]. [~args:`Zero] means no holes (literal); [~args:`One]
      means one hole (wraps the inner parser/printer). *)
  val expr_of_format
    :  loc:Location.t
    -> Format_string.t
    -> args:[ `Zero | `One ]
    -> expression

  (** Whether the [@chars ...] attribute should be accessed (if present, appends "_chars"
      to the derived name from a type, and supplies a [~chars] argument ). *)
  val reads_chars_attr : bool

  (** Function name prefix. Used to derive binding names like [unstring_foo]. *)
  val prefix : string

  (** What the expected type for the derived expression is (parametrised). *)
  val expected_type : loc:Location.t -> core_type -> core_type

  (** The attribute to read the custom override from. *)
  val custom_attribute : (core_type, expression) Attribute.t
end

(** Given a code-generation strategy, produce the full deriver machinery. *)
module Make (_ : S) : sig
  (** Generate an expression from a [core_type] by recursively walking its structure.
      [type_params] maps type variable names to the parameter names used in the generated
      function (e.g. ['a] -> ["a"]). *)
  val of_type
    :  type_params:(label, label, _) Base.Map.t
    -> loc:Location.t
    -> core_type
    -> expression

  (** Handle the [[%unstring ("format" : type)]] extension point syntax. *)
  val extension
    :  format:Format_string.t
    -> type_:core_type
    -> loc:Location.t
    -> expression

  (** Generate the [let unstring = ...] or [let restring = ...] structure item for a
      [@@deriving unstring] type declaration. *)
  val deriver : type_decl:type_declaration -> loc:Location.t -> structure_item

  (** Generate the corresponding signature item ([val unstring : ...] . *)
  val sig_item : type_decl:type_declaration -> loc:Location.t -> signature_item

  (** Map a type name to its binding name: ["t" -> prefix], ["foo" -> prefix ^ "_foo"]. *)
  val binding_name : string -> string
end
