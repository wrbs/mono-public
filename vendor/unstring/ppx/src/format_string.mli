open! Base
open! Ppxlib

(** A format string with [%{...}] interpolations, as it appears in source code.

    To include a literal [%] in the format string, double it: [%%]. For example, [%%{]
    produces a literal [%{] in the output.

    {2 Hole syntax}

    Holes are written as [%{name}] where [name] determines matching:

    - [%{_}] — positional hole (shorthand when there is exactly one).
    - [%{1}], [%{2}], ... — numbered holes, 1-indexed. Correspond to positional entries in
      the [Fields.Many.parsers] list.
    - [%{x}], [%{y}], ... — named holes. Correspond to [Some name] entries in the
      [Fields.Many.parsers] list. *)
type t =
  { text : string
  ; loc : Location.t (** Location of the string literal in source *)
  }

(** Describes the holes in a format string and how their parsed values are assembled.

    - [None]: the format string has no holes. The result is [unit Unstring.t] (via
      [Unstring.all_unit]).
    - [Singleton]: the format string has exactly one positional hole ([%{_}] or [%{1}]).
      The result is the parser's value directly, without any wrapping.
    - [Many]: the format string has multiple holes, each with an associated parser and an
      explicit [combine] function. Each element of [parsers] is [(label, parser_expr)]
      where [label] is [None] for positional (numbered) holes or [Some name] for named
      holes. Positional entries are numbered 1, 2, ... in list order. [combine] receives a
      list of [(label option, variable_expression)] pairs — one per hole in [parsers]
      order — and must return the body expression. The result is wrapped in
      [Unstring.map_hlist]. *)
module Fields : sig
  type t =
    | None
    | Singleton of { parser : expression }
    | Many of
        { parsers : (string option * expression) list
        ; combine : (string option * expression) list -> expression
        }
end

(** Build an [Unstring.t] expression from a format string and its fields.

    [loc] is the location to use for generated AST nodes (e.g., the surrounding
    expression). The format string's own [t.loc] is used for error reporting and points at
    the string literal as a whole. *)
val build_unstring : t -> loc:Location.t -> fields:Fields.t -> expression

(** Build a [string]-typed expression from a format string and to_string expressions for
    each hole.

    Each element of [hole_string_exprs] is [(label, expr)] where [label] is [None] for
    positional (numbered) holes or [Some name] for named holes, and [expr] is a
    [string]-typed expression. Positional entries are numbered 1, 2, ... in list order.

    The result concatenates literals and hole expressions via [String.concat]. *)
val build_string
  :  t
  -> loc:Location.t
  -> hole_string_exprs:(string option * expression) list
  -> expression
