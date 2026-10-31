open! Core

(** A concise, regex-like rendering of grammar shapes for inspection and testing.

    Adjacent literal characters are merged into strings and quoted with sexp conventions
    when they contain special characters (whitespace, parens, etc.).

    Long concatenations are line-broken via [Format]; use [pp] for width-aware output.

    Meta-characters:
    - Terminals: [·] (any), [⟦...⟧] (character class, e.g. [⟦\d⟧], [⟦\a⟧], [⟦\s⟧]),
      [⟦^...⟧] (negated character class)
    - Structure: [""] (empty/epsilon), [∅] (no match), [‖] (alternation), [⋆] (star)
    - Grouping: [()] (precedence), [«»] (capture) *)

(** A simplified AST that retains only the "shape" of a grammar — the structure that
    determines what strings can match, without any value-level semantics (maps, folds,
    captures' return types, etc.).

    During construction via [of_grammar], the tree is normalized: nested [Concat]s are
    flattened, and adjacent single-character terminals are merged into [Literal]s. *)
type t =
  | Literal of string
  | Terminal of Char_filter.t
  | Capture of t
  | Union of t list
  | Concat of t list
  | Star of t
[@@deriving sexp_of]

(** Translates a grammar into its normalized shape. *)
val of_grammar : _ Defs.Grammar.t -> t

(** Pretty-prints the shape in the compact regex-like notation described in [glossary].
    Uses [Format] boxes so that long concatenations and unions are line-broken when the
    output exceeds the formatter's margin. *)
val pp : Stdlib.Format.formatter -> t -> unit

(** [to_string t] is equivalent to pretty-printing with the default margin. *)
val to_string : t -> string

(** Returns a human-readable glossary of all symbols used in the rendering. *)
val glossary : string
