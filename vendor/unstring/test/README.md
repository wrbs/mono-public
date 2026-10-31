# Unstring Tests

Tests for the `Unstring` parser-combinator library.

## Test files

### unit_tests.ml

Fixed-input expect tests exercising each public constructor in isolation:
`return`, `lit`, `char` (with various filters), `capture`, `map`, `map2`,
`star`, `multiple` (bounded and empty ranges), `list`, `choice`, `map_hlist`,
`run_unambiguous`, and `make_of_string`.

### ordering_tests.ml

Expect tests that pin the parse ordering for ambiguous grammars. Since the
ordering is stable but non-obvious ("right-lexicographic"), these tests use
`capture` to make segment lengths visible and snapshot the full sequence of
parses for small inputs.

### equivalence_tests.ml

Quickcheck tests using `Blueprint` to generate random parser trees. Each test
declares a pair of constructions (`lhs` and `rhs`) that should produce
identical parse results on all inputs. Covers:
- Applicative laws (associativity, left/right identity, naturality)
- Functor laws (identity, composition)
- Constructor equivalences (`star ≡ multiple`, `star ≡ fold`, `multiple ≡ fold`,
  `list ≡ fold + rev`, `capture ≡ capture'`, `lit ≡ char-by-char`)

### concrete_property_tests.ml

Quickcheck tests for individual constructors with small, concrete generators
(no `Blueprint`). Each test verifies a specific semantic property: `lit`
matches exactly its string, `star` accepts homogeneous strings and rejects
foreign characters, `choice` matches iff the input is an alternative, `map2`
matches concatenations, and `list` (with and without separator) collects the
right number of items given bounds.

## Support modules

### blueprint.ml

Defines `Blueprint.t`, a type that mirrors `Unstring.t` constructors and can
be randomly generated via quickcheck. Provides conversions to `Unstring.t`
(via `to_unstring` / `to_unit_unstring`) and a matching string generator
(`matching_string_gen`) that produces strings likely to parse successfully.

### string_gen.ml

Combinators for building `string Quickcheck.Generator.t` values: `concat`,
`star`, `repeat`, `choice`, etc. Used by `blueprint.ml` to keep the matching
string generation logic readable.

### common.ml

Thin wrappers around `Unstring.compile` + `Unstring.Compiled.run_*` to reduce
boilerplate in test files.
