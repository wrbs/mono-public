open! Core

module Definitions = struct
  module Fold = struct
    type ('acc, 'a) t =
      { init : 'acc
      ; f : 'acc -> 'a -> 'acc
      }
  end

  module Capture = struct
    type ('a, 'input) t = 'input -> pos:int -> len:int -> 'a
  end
end

(** The grammar representation for the parsing engine.

    This module defines GADTs for representing typed grammars (regular expressions with
    typed capture groups). The grammar constructors correspond to standard regex
    operations:

    - [Return], [Map]: Pure values and transformations
    - [Terminal]: Matches a single token (like a character class)
    - [Union]: Alternation (regex [|])
    - [Append], [Concat_unit]: Concatenation
    - [Star], [Star_unit]: Kleene star (regex [*])
    - [Capture], [Capture_also]: Capture groups that extract segments

    {2 Formal Semantics}

    {3 Glossary}

    {v
    ts          token sequence
    ε           empty token sequence
    ts₁ · ts₂   concatenation of token sequences
    ⟦ts⟧        segment representation of a token sequence
    g ⇓ ts ⇒ v  grammar g matches token sequence ts, producing value v
    v}

    The semantics is relational: a grammar may produce multiple values for a given input
    (via [Union] or [Star]), corresponding to different parse trees.

    {3 Core Semantics}

    {v
    ─────────────────────
    Return x ⇓ ε ⇒ x


    matches t tok
    ─────────────────────
    Terminal t ⇓ [tok] ⇒ ()


    g ⇓ ts ⇒ v
    ─────────────────────
    Map (g, f) ⇓ ts ⇒ f v


    g₁ ⇓ ts₁ ⇒ v₁    g₂ ⇓ ts₂ ⇒ v₂
    ───────────────────────────────
    Append (g₁, g₂, f) ⇓ ts₁ · ts₂ ⇒ f v₁ v₂


    gᵢ ∈ gs    gᵢ ⇓ ts ⇒ v
    ───────────────────────
    Union gs ⇓ ts ⇒ v


    ──────────────────────────────
    Star (g, fold) ⇓ ε ⇒ fold.init


    Star (g, fold) ⇓ ts₁ ⇒ acc    g ⇓ ts₂ ⇒ v
    ─────────────────────────────────────────
    Star (g, fold) ⇓ ts₁ · ts₂ ⇒ fold.f acc v


    g ⇓ ts ⇒ v
    ─────────────────────────────────
    Capture_also (g, f) ⇓ ts ⇒ (f ⟦ts⟧) v
    v}

    {3 Derived Forms}

    The following constructors are optimizations. Their semantics can be expressed in
    terms of the core constructors:

    {v
    Concat_unit [g₁; g₂; ...]  ≡  folding Append with (fun () () -> ())
    Star_unit g                ≡  Star (g, {init = (); f = fun () () -> ()})
    Capture (g, f)             ≡  Capture_also (g, λseg. λ(). f seg)
    v}

    {3 Tests}

    Those semantics are being property-tested directly in test/semantics_tests.ml *)
module type Derived = sig
  type terminal
  type input

  type 'a t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Terminal : terminal -> unit t
    | Capture : unit t * ('a, input) Definitions.Capture.t -> 'a t
    | Capture_also : 'a t * ('a -> 'b, input) Definitions.Capture.t -> 'b t
    | Union : 'a t list -> 'a t
    | Append : 'a t * 'b t * ('a -> 'b -> 'c) -> 'c t
    | Concat_unit : unit t list -> unit t
    | Star : 'a t * ('acc, 'a) Definitions.Fold.t -> 'acc t
    | Star_unit : unit t -> unit t
  [@@deriving sexp_of]
end

module type Token_lookup = sig
  type token
  type 'a t [@@deriving sexp_of]

  val create : (token -> 'a) -> 'a t
  val find : 'a t -> token -> 'a
end

(** The grammar module signature required by the parsing engine. *)
module type S = sig
  type token [@@deriving sexp_of]
  type terminal [@@deriving sexp_of]
  type input [@@deriving sexp_of]

  (** Bring-your-own token lookup data structure. Since the most efficient shape for it
      might depend on the token type in question, its implementation is left to the
      caller. *)
  module Token_lookup : Token_lookup with type token := token

  val matches : terminal -> token -> bool

  include Derived with type terminal := terminal and type input := input
end

module type Grammar_module_types = sig
  module Definitions = Definitions
  include module type of Definitions

  module type Derived = Derived
  module type Token_lookup = Token_lookup
  module type S = S

  module Make_derived (M : sig
      type terminal [@@deriving sexp_of]
      type input [@@deriving sexp_of]
    end) : Derived with type terminal := M.terminal and type input := M.input
end
