open! Core

type 'a t =
  | Simple of 'a Sequence.t
  | Interleaved of 'a Sequence.t list

module type Sexpable_comparable = sig
  type t [@@deriving compare, sexp_of]

  include Comparable.S_plain with type t := t
end

(** [test (module M) actual expected ~cutoff] tests that the parse sequence [actual]
    matches [expected]:

    - if [expected] is [Simple _], that the sequences are the same (up to [cutoff])
    - if [expected] is [Interleaved _], that the set of the parses is the same, and that
      the first parse in [actual] is one of the first parses in [expected].

    For potentially infinite sequences, we clip to [cutoff] elements and do partial
    validation:
    - For [Simple]: compares the first [cutoff] elements; returns [`Inconclusive] if
      prefixes match but at least one sequence exceeds [cutoff]
    - For [Interleaved]:
      - Always checks first-parse agreement
      - If both bounded by [cutoff]: full set equality
      - If one bounded and one not: length mismatch error
      - If both unbounded: cannot verify sets, only first-parse

    Returns:
    - [`Ok] if full validation passes
    - [`Error _] if any check fails
    - [`Inconclusive] if partial validation passes but full verification wasn't possible *)
val test
  :  (module Sexpable_comparable with type t = 'a)
  -> actual:'a Sequence.t
  -> expected:'a t
  -> cutoff:int
  -> [ `Ok | `Error of Error.t | `Inconclusive ]
