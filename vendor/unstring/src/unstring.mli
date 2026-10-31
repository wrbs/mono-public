open! Core

(** [Unstring], a parser-combinator library for writing low-effort, typeful, composable
    string parsers.

    The core type is ['a Unstring.t], which represents a blueprint for a parser from
    [string] to ['a]. One can build it via various constructors; then [compile] it and
    then use the ['a Compiled.t] to parse strings.

    The engine backing [Unstring] is a homegrown OCaml regex engine, made with the ease of
    extracting data in mind. It is not terribly slow in the common case, but it definitely
    isn't blazingly fast (and allocates a fair amount); if performance is your top concern
    you should probably pick a different solution. At the same time, the worst-case time
    complexity is something like O(|input| * |parser|), so it is safe to use even with
    highly ambiguous parsers.

    [Unstring] aims to help users be thorough when it comes to handling accidental
    ambiguity in the parsing grammar, hence the parser is able to return a full sequence
    of parse results instead of just an arbitrary one. Alternatively,
    [Compiled.run_unambiguous] returns an [Ok _] only if it is unambiguous, so that the
    caller can conservatively reject a plausible misparse. Read the [Compiled] module for
    more details.

    The order in which parse results are returned is stable, but complicated to explain.
    We would strongly advise against trying to reason about which parse is going to be the
    first one if it matters to you. However, you can assume that the order in which the
    parse results are returned is not going to change across executable versions, and
    breaking changes will involve explicit versioning and migrations.

    (The order is vaguely "right-lexicographic": assume that each parse result has a list
    of indices assigned to it based on which parts of the expression were matched; then
    the parses are returned in lexicographic order, if read from the right. Concretely, if
    presented with something like
    {[
      all_unit [ choice [ lit "ab"; lit "a" ]; choice [ lit "bc"; lit "c" ] ]
    ]}
    on the string ["abc"], the first parse is going to split like ["(a)(bc)"], not
    ["(ab)(c)"], since the former matches on the earlier item in the rightmost [choice].) *)

(** Represents a blueprint for a parser from [string] to ['a]. It can be built with
    various composable constructors, and then [compile]d when ready. *)
type 'a t

module Compiled : sig
  (** The compiled parser, ready to use. *)
  type 'a t

  (** [run_unambiguous sexp_of compiled str] returns:
      - [None] if the parser doesn't match at all
      - [Some (Ok x)] if [str] unambiguously parses to [x]
      - [Some (Error _)] if one of the closures inside the parser (e.g. the [f] in
        [map ~f]) raised during value reconstruction, or if multiple valid parses have
        been reported. Note that an error value still means that the parser matches.

      [sexp_of] is used inside the "ambiguous parse" error message; providing
      [[%sexp_of: _]] in its place is a valid choice. *)
  val run_unambiguous : ('a -> Sexp.t) -> 'a t -> string -> 'a Or_error.t option

  (** [run_all_parses compiled str] produces a sequence of all valid parses; those are
      ['a Or_error.t] to represent the possibility of an exception being raised during
      value reconstruction.

      The sequence is computed lazily and can be infinite, e.g.
      {[
        run_all_parses (list (return ()) |> compile) ""
      ]}
      returns a sequence of [[]], [[()]], [[();()]], etc. *)
  val run_all_parses : 'a t -> string -> 'a Or_error.t Sequence.t

  (** The expert interface for a more fine-grained control of the parser execution. *)
  module Expert = Compiled_expert
end

(** Compiles the parser. [compile] can be an expensive operation, a couple orders of
    magnitude more so than parsing itself. It should either be explicitly run as a
    separate step (for example, at the start of the program), or wrapped in a [lazy] (cf.
    [make_of_string] below). *)
val compile : 'a t -> 'a Compiled.t

(** Creates an [of_string] deserialiser compatible with the standard conventions.

    The [compile_when] argument determines whether the underlying call to [compile] should
    run immediately, or lazily as needed upon first deserialisation. Thus if you want to
    write
    {[
      let of_string = Unstring.make_of_string unstring
    ]}
    in the toplevel, you probably should pass [~compile_when:`Lazily] to keep executable
    startup times in check.

    The [on_ambiguity] argument determines what the deserialiser should do in the event of
    the parser reporting multiple valid parsers. The sexp serialiser passed to [`Raise]
    will be invoked to produce the exception message in case of ambiguity. *)
val make_of_string
  :  'a t
  -> compile_when:[ `Now | `Lazily ]
  -> on_ambiguity:[ `Raise of 'a -> Sexp.t | `Pick_first ]
  -> (string -> 'a) Staged.t

(** {2 Constructors}

    An ['a Unstring.t] is semantically equivalent to [string -> 'a Sequence.t] (exception
    handling aside). The constructors can be thought of as particular ways of combining
    those functions, even though the actual representation is a lot more efficient.

    The constructors follow an applicative interface, where [both]/[map2] concatenate the
    two parsers in order together; for example the parser
    {[
      let%map_open.Unstring () = lit "the quick brown "
      and animal_1 = capture (multiple ~bounds:(Incl 1, Unbounded) (char Alpha))
      and () = lit " jumps over the lazy "
      and animal_2 = string in
      animal_1, animal_2
    ]}
    matches strings of the form `"the quick brown ANIMAL_1 jumps over the lazy ANIMAL_2"`.

    For efficiency and ergonomics reasons, the [char] primitive doesn't return the matched
    character, only a [unit]; in effect a binary "matches/does not match". To extract
    meaningful data from the string one has to use [capture], which returns the substring
    the sub-parser matched successfully on.

    That being said, [ppx_unstring] is meant to be the primary way of constructing an
    [Unstring.t]; the construction interface in this file ideally should be used only
    sparingly, when the desired parser does not fit in the shape [ppx_unstring] is
    designed for.

    [Unstring] does not currently support recursive grammars, but it might be extended so
    in the future. *)

include Applicative.S with type 'a t := 'a t

(** Shorthands for specifying the set of accepted characters for matching on a single
    character in the input string. A [Char_filter.t] is morally equivalent to a
    [char -> bool]. *)
module Char_filter : sig
  type t =
    | Any
    | Digit
    | Alpha
    | Whitespace
    | Char of char
    | Fun of (char -> bool)
    | Not of t

  val matches : t -> char -> bool
end

module Bound : sig
  type t = int Maybe_bound.t [@@deriving sexp_of]
end

(** Heterogeneous list for [_ Unstring.t] *)
module Hlist : Heterogeneous_list.S1 with type 'a Element.t := 'a t

module Open_on_rhs_intf : sig
  module type S = sig
    (** [char filter] matches a single character in the input as determined by the filter *)
    val char : Char_filter.t -> unit t

    (** [lit str] matches the string [str] verbatim *)
    val lit : string -> unit t

    (** [star t] matches [t] zero or more times in succession *)
    val star : unit t -> unit t

    (** NOTE: A number of constructors below accept an optional [bounds] argument. That
        argument can be costly in that it causes multiple copies of the underlying parser
        to be instantiated. For example,
        [multiple ~bounds:(Incl 100_000, Incl 1000_000) t] would make 1000_0000 copies of
        [t] and thus take very long to compile. *)

    (** [multiple ?sep ?bounds t] is like a [star t], except that it also supports bounds
        on the number of repeats and an optional separator. (NOTE See the note on bounds) *)
    val multiple : ?sep:unit t -> ?bounds:Bound.t * Bound.t -> unit t -> unit t

    (** [capture t] matches a string iff [t] does; however, [capture t] also returns the
        matched string in question *)
    val capture : unit t -> string t

    (** [capture' t] is a generalised version of [capture]. The provided [f] gets fed the
        underlying parsed value, the entire string being parsed, and the indices of the
        matched substring. i.e. [capture t] is equivalent to
        [capture' t ~f:(fun () ~input ~pos ~len -> String.sub input ~pos ~len)]. *)
    val capture' : 'a t -> f:('a -> input:string -> pos:int -> len:int -> 'b) -> 'b t

    (** [list t] is like a [multiple t], except that it collects the results into a list.
        (NOTE see the note on bounds) *)
    val list : ?sep:unit t -> ?bounds:Bound.t * Bound.t -> 'a t -> 'a list t

    (** [fold] is like [list], except that it supports arbitrary folding. (NOTE see the
        note on bounds) *)
    val fold
      :  ?sep:unit t
      -> ?bounds:Bound.t * Bound.t
      -> 'a t
      -> init:'acc
      -> f:('acc -> 'a -> 'acc)
      -> 'acc t

    (** [choice ts] matches iff any of the [ts] match. *)
    val choice : 'a t list -> 'a t

    (** [map_hlist ts ~f] concatenates the [ts] parsers, and applies [f] to the resulting
        heterogeneous list of parse results if matched *)
    val map_hlist : 'shape Hlist.t -> f:('shape Heterogeneous_list.t -> 'a) -> 'a t

    (** [option some ~none] is equivalent to
        [choice [ map some ~f:(fun x -> Some x); map none ~f:(fun () -> None) ]] *)
    val option : 'a t -> none:unit t -> 'a option t

    (** {3 Parsers for common types}

        These parsers are designed to roundtrip with the corresponding standard
        [to_string] functions: for any [x], [parse (T.to_string x) = x].

        The grammars are intentionally conservative; for example, [int] matches only one
        or more digits [0-9], optionally prefixed by a sign [-+]. This is so that they can
        be easily used with common delimiters and separators; [list ~sep:(lit ",") int]
        parses naturally. *)

    val bool : bool t
    val int : int t
    val float : float t
    val string : ?chars:Char_filter.t -> unit -> string t
  end
end

include Open_on_rhs_intf.S

include
  Applicative.Let_syntax
  with module Open_on_rhs_intf := Open_on_rhs_intf
   and type 'a t := 'a t

module Export : sig
  (** {2 Named parsers for ppx_unstring}

      These follow the naming convention that ppx_unstring uses for type-directed lookup.
      For a type [foo], the ppx looks up [unstring_foo]. *)

  val unstring_int : int t
  val unstring_float : float t
  val unstring_bool : bool t
  val unstring_string : string t
  val unstring_string_chars : chars:Char_filter.t -> string t
  val unstring_unit : unit:unit t -> unit t
  val unstring_list : sep:unit t -> 'a t -> 'a list t
  val unstring_option : some:('a t -> 'a t) -> none:unit t -> 'a t -> 'a option t

  (** {2 Named restring functions for ppx_unstring}

      These follow the naming convention that ppx_unstring uses for type-directed
      [~restring] lookup. For a type [foo], the ppx looks up [restring_foo]. *)

  val restring_int : int -> string
  val restring_float : float -> string
  val restring_bool : bool -> string
  val restring_string : string -> string
  val restring_unit : unit:string -> unit -> string
  val restring_list : sep:string -> ('a -> string) -> 'a list -> string

  val restring_option
    :  some:(('a -> string) -> 'a -> string)
    -> none:string
    -> ('a -> string)
    -> 'a option
    -> string
end

(** A compact, regex-like rendering of the shape of a parser for inspection and snapshot
    testing. The shape captures the matching structure of the grammar (literals, character
    classes, alternation, repetition, captures) while discarding value-level semantics.

    See [glossary] for the notation used. *)
module Printable_shape : sig
  (** A simplified grammar AST. Adjacent single-character terminals are merged into
      [Literal]s, and nested [Concat]s are flattened. *)
  type t =
    | Literal of string
    | Terminal of Char_filter.t
    | Capture of t
    | Union of t list
    | Concat of t list
    | Star of t
  [@@deriving sexp_of]

  (** Pretty-prints the shape in a compact regex-like notation. Uses [Format] boxes so
      that long expressions are line-broken at the formatter's margin. *)
  val pp : Stdlib.Format.formatter -> t -> unit

  (** [to_string t] is equivalent to pretty-printing with the default margin. *)
  val to_string : t -> string

  (** Returns a human-readable glossary of all symbols used in the rendering. *)
  val glossary : string
end

(** Returns the printable shape of the parser's underlying grammar. *)
val to_printable_shape : _ t -> Printable_shape.t

(** Prints the shape of the underlying grammar. Useful for snapshot tests that assert the
    grammar hasn't changed unexpectedly.

    Shorthand for
    [fun t -> t |> to_printable_shape |> Printable_shape.to_string |> print_endline] *)
val print_shape : _ t -> unit
