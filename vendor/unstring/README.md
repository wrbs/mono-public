# Unstring

`Unstring` is a library for building low-effort, typeful, composable string parsers.

Its main design points are:
- easy ad-hoc use: It is very simple to write a parser given an example string.
- typeful, composable, straightforward semantics: An `'a Unstring.t` morally corresponds to
  a `string -> 'a Sequence.t`, and there are no surprises in composing `Unstring.t`s.
- explicit ambiguity handling: Parsers are allowed to be ambiguous, and if they return
  multiple parses, it is up to the user whether to pick one arbitrarily or return an error.
- reasonable performance: within an order of magnitude of Re.

See `src/unstring.mli` for the full API documentation and usage examples.

### Code organization

- `src/` - the public library interface and implementation.
- `engine/` - the low-level engine implementation.

# ppx_unstring

A PPX for building Unstring parsers from format strings and type definitions.

## Quick examples

```ocaml
open Core
open Unstring.Export
```

Parse a structured record out of a string:

```ocaml
module Point = struct
  type t = { x : int; y : int }
  [@@deriving sexp_of, unstring ~of_string ~on_ambiguity:`Raise]
  [@@format "(%{x},%{y})"]
end
```

```ocaml
# Point.of_string "(10,20)"
- : Point.t = {Point.x = 10; y = 20}
```

Define a variant with string representations and get parsing for free:

```ocaml
module Shape = struct
  type t =
    | Circle of { center : Point.t; radius : int } [@format "circle at %{center} r=%{radius}"]
    | Rectangle of Point.t * Point.t [@format "rect %{1} to %{2}"]
    [@@deriving sexp_of, unstring ~of_string ~on_ambiguity:`Raise]
end
```

```ocaml
# Shape.of_string "circle at (0,0) r=5"
- : Shape.t = Shape.Circle {Shape.center = {Point.x = 0; y = 0}; radius = 5}
# Shape.of_string "rect (-2,-1) to (2,1)"
- : Shape.t = Shape.Rectangle ({Point.x = -2; y = -1}, {Point.x = 2; y = 1})
```

Ad-hoc parsing without type definitions:

```ocaml
let parser = [%unstring ("name:%{name}, age:%{age}" : name:string * age:int)]
```

```ocaml
# Unstring.Compiled.run_unambiguous [%sexp_of: _] (Unstring.compile parser) "name:Alice, age:30"
- : (name:string * age:int) Or_error.t option =
Some (Core__.Result.Ok (~name:"Alice", ~age:30))
```

## Using a parser

An `'a Unstring.t` is a parser blueprint. To use it:

```ocaml
(* Compile the parser (expensive; do this once, not per input) *)
let compiled = Unstring.compile parser;;

(* Parse a string *)
match Unstring.Compiled.run_unambiguous [%sexp_of: _] compiled "" with
  | None -> [%message "No parse"]
  | Some (Ok (~name, ~age)) -> [%message "Ok" (name : string) (age : int)]
  | Some (Error error) ->
    [%message "Ambiguous parse, or exception during reconstruction" (error : Error.t)]
;;
```

For a simpler interface, derive `of_string` (see below) or use `Unstring.make_of_string`:

```ocaml
let of_string =
  Unstring.make_of_string parser
    ~compile_when:`Lazily
    ~on_ambiguity:(`Raise [%sexp_of: _])
  |> Staged.unstage
```

The result is `(string -> t) Staged.t` — it is staged so that parser compilation can be
deferred to first use.

## Format string syntax

Format strings use `%{...}` for interpolation holes:

| Placeholder | Usage |
|-------------|-------|
| `%{_}` | Single value |
| `%{1}`, `%{2}`, ... | Tuple elements by position (1-indexed) |
| `%{name}` | Named field (records or labeled tuples) |

Double `%%` are translated into singular `%`, so that the literal `%{` can be represented
by writing `%%{`

```ocaml
module Hole = struct
  type t = { content : string }
  [@@deriving unstring] [@@format "%%{%{content}}"]
end
```

```ocaml
# Unstring.print_shape Hole.unstring
%{ «·⋆» }
- : unit = ()
```

## The extension: `[%unstring ...]`

### Format string with type: `[%unstring ("format" : type)]`

For ad-hoc parsing. The format string determines the shape; the type annotation determines the result type.

```ocaml
let run unstring s =
Staged.unstage
 (Unstring.make_of_string unstring ~compile_when:`Now ~on_ambiguity:(`Raise [%sexp_of: _]))
 s
;;
```

```ocaml
# run [%unstring ("x=%{_}" : int)] "x=42"
- : int = 42
# run [%unstring ("(%{1},%{2})" : int * string)] "(42,hello)"
- : int * string = (42, "hello")
# run [%unstring ("name:%{name}, age:%{age}" : name:string * age:int)] "name:Alice, age:30"
- : name:string * age:int = (~name:"Alice", ~age:30)
# run [%unstring "hello world"] "hello world"
- : unit = ()
```

Tuple arguments are accepted out of order too:
```ocaml
# run [%unstring ("%{2} then %{1}" : int * string)] "hello then 42"
- : int * string = (42, "hello")
```

### Type-directed lookup: `[%unstring: type]`

The ppx resolves types to parsers by naming convention:
- `int` → `unstring_int` (from `Unstring.Export`)
- `Foo.t` → `Foo.unstring`
- `Foo.bar` → `Foo.unstring_bar`

```ocaml
open Unstring.Export  (* brings unstring_int, unstring_string, etc. into scope *)
```

```ocaml
# [%unstring: int], [%unstring: string], [%unstring: float], [%unstring: bool]
- : int Unstring.t * string Unstring.t * float Unstring.t * bool Unstring.t =
(<abstr>, <abstr>, <abstr>, <abstr>)
```

expands to

```ocaml
# unstring_int, unstring_string, unstring_float, unstring_bool
- : int Unstring.t * string Unstring.t * float Unstring.t * bool Unstring.t =
(<abstr>, <abstr>, <abstr>, <abstr>)
```

For custom types, define a value following the naming convention:

```ocaml
module Color = struct
  type t = Red | Green | Blue

  let unstring =
    Unstring.choice
      [ Unstring.(lit "red" |> map ~f:(fun () -> Red))
      ; Unstring.(lit "green" |> map ~f:(fun () -> Green))
      ; Unstring.(lit "blue" |> map ~f:(fun () -> Blue))
      ]
end
```

```ocaml
# [%unstring: Color.t]
- : Color.t Unstring.t = <abstr>
```


### Parameterized types

For types with parameters like `'a Foo.t`, the ppx calls the unstring function with parsers for each type argument:

```ocaml
# module Wrapper = struct
    type 'a t =  { value : 'a }
    [@@deriving unstring] [@@format "wrapped<%{value}>"]
  end
module Wrapper :
  sig
    type 'a t = { value : 'a; }
    val unstring : 'a Unstring.t -> 'a t Unstring.t
  end

# Unstring.print_shape [%unstring: bool Wrapper.t]
wrapped< (false ‖ true) >
- : unit = ()
```

### Attributes on types

#### `[@chars]` on strings

Restrict which characters a string can match:

```ocaml
let digit_str = [%unstring: (string [@chars Digit])];; (* only digits *)
let letter_str = [%unstring: (string [@chars Alpha])];; (* only letters *)
let not_ws_str = [%unstring: (string [@chars Not Whitespace])];;  (* no whitespace *)
let hyphen_only_str = [%unstring: (string [@chars Char '-'])];;  (* only hyphens *)
```

The full specification is at `Unstring.Char_filter.t`.

#### `[@some]` and `[@none]` on options

Both are required:

```ocaml
let option_example = [%unstring: (int option [@some "(%{_})"] [@none "none"])];;
```

```ocaml
# run option_example "(42)"
- : int option = Some 42
# run option_example "none"
- : int option = None
```

#### `[@unit]` on unit

Specifies a literal that matches a unit value:

```ocaml
let unit_example = [%unstring: (unit [@unit "()"])];;
```

```ocaml
# run unit_example "()"
- : unit = ()
```

It is especially useful when parsing nested types such as `unit option`:

```ocaml
let unit_option_example = [%unstring: ((unit [@unit "()"]) option [@some "Some %{_}"] [@none "None"])];;
```

```ocaml
# run unit_option_example "Some ()";;
- : unit option = Some ()
# run unit_option_example "None";;
- : unit option = None
```

#### `[@sep]` on lists

Specifies the separator between elements:

```ocaml
# run [%unstring: (int list [@sep ","])] "1,2,3";;
- : int list = [1; 2; 3]
```

#### `[@format]` on tuples

Tuple types require a `[@format]` attribute:

```ocaml
# run [%unstring: (int * int [@format "(%{1},%{2})"])] "(1,2)"
- : int * int = (1, 2)
# run [%unstring: (x:int * y:int [@format "x=%{x}, y=%{y}"])] "x=5, y=10"
- : x:int * y:int = (~x:5, ~y:10)
```


#### `[@format]` on polymorphic variants

Each row requires `[@format]`. Polymorphic variants with arguments take the attribute on
the argument level:

```ocaml
# run [%unstring: [ `On [@format "on"] | `Off [@format "off"] ]] "on"
- : [> `Off | `On ] = `On
# run [%unstring: [ `Value of (int[@format "value=%{_}"]) | `Empty [@format "empty"] ]] "value=42"
- : [> `Empty | `Value of int ] = `Value 42
```


#### `[@custom]` for overriding

Override the default parser lookup with a custom expression:

```ocaml
# let my_bool = Unstring.(lit "yes" |> map ~f:(fun () -> true)) in
  run [%unstring: (bool [@custom my_bool])] "yes"
- : bool = true
```

When using `[%unstring.with_restring ...]`, provide a corresponding `[@custom_restring]`
attribute for the restring function:

```ocaml
# let my_bool = Unstring.(lit "yes" |> map ~f:(fun () -> true))
  and my_restring b = if b then "yes" else "no" in
  let _unstring, restring =
    [%unstring.with_restring: (bool [@custom my_bool] [@custom_restring my_restring])]
  in
  restring true
- : string = "yes"
```

### Getting both parser and restring: `[%unstring.with_restring ...]`

Returns a pair `('a Unstring.t * ('a -> string))` — the parser and the corresponding
restring function — in a single expression.

Works with both type-directed lookup and format strings:

```ocaml
# let unstring, restring = [%unstring.with_restring: int] in
  let parsed = Staged.unstage (Unstring.make_of_string unstring ~compile_when:`Now ~on_ambiguity:(`Raise [%sexp_of: _])) "42" in
  print_s [%sexp (parsed : int)];
  print_endline (restring 42)
42
42
- : unit = ()
```

```ocaml
# let unstring, restring = [%unstring.with_restring ("(%{1},%{2})" : int * string)] in
  let parsed = Staged.unstage (Unstring.make_of_string unstring ~compile_when:`Now ~on_ambiguity:(`Raise [%sexp_of: _])) "(42,hello)" in
  print_s [%sexp (parsed : int * string)];
  print_endline (restring (42, "hello"))
(42 hello)
(42,hello)
- : unit = ()
```

Attributes like `[@some]`, `[@none]`, `[@sep]`, and `[@unit]` are supported on both forms,
just as with `[%unstring ...]`. The only exception is `[@custom ..]`, for restring, you
should provide `[@custom_restring ..]`.

## The deriver: `[@@deriving unstring]`

### Records

Records require a `[@@format]` attribute with `%{field}` placeholders:

```ocaml
# type point = { x : int; y : int }
  [@@deriving unstring] [@@format "(%{x},%{y})"]
type point = { x : int; y : int; }
val unstring_point : point Unstring.t = <abstr>
```

Field types accept and require attributes the same way as in the extension:

```ocaml
type entry = { label : (string [@chars Alpha]); values : (int list[@sep ","]) }
[@@deriving unstring] [@@format "%{label}:%{values}"]
```

### Variants

Each constructor requires a `[@format]` attribute. Use `%{_}` for single payloads, `%{1}`, `%{2}` for tuple payloads, or `%{field}` for inline records:

```ocaml
type command =
  | Move of int [@format "move %{_}"]
  | Goto of int * int [@format "goto %{1} %{2}"]
  | Click of { x : int; y : int } [@format "click(%{x},%{y})"]
  | Stop [@format "stop"]
[@@deriving unstring]
```

### Polymorphic variants

Same as inline polymorphic variants, but as a type definition.

```ocaml
type toggle = [ `On [@format "on"] | `Off [@format "off"] ]
[@@deriving unstring]
```

Polymorphic variant inheritance works:

```ocaml
type base = [ `A [@format "a"] | `B [@format "b"] ]
[@@deriving unstring ~restring]
```

```ocaml
# run [%unstring: [ base | `C [@format "c"] ]] "a";;
- : [> `A | `B | `C ] as '_weak3 = `A
```

Restring also works with inherited rows via `[%unstring.with_restring ...]`:

```ocaml
# let _unstring, restring = [%unstring.with_restring: [ base | `C [@format "c"] ]] in
  restring `A
- : string = "a"
```

### Type aliases

For type aliases, the ppx delegates to the underlying type's parser:

```ocaml
type my_int = int [@@deriving unstring]
```

### Parameterized types

The generated `unstring` is a function taking parsers for each parameter:

```ocaml
# type 'a wrapper = { value : 'a }
  [@@deriving unstring] [@@format "%{value}"]
type 'a wrapper = { value : 'a; }
val unstring_wrapper : 'a Unstring.t -> 'a wrapper Unstring.t = <fun>
```

### Signature generation

`[@@deriving unstring]` works in `.mli` files and module signatures:

```ocaml
# module type S = sig
  type t [@@deriving unstring]
  type foo [@@deriving unstring]
  type 'a wrapper [@@deriving unstring]
  end
module type S =
  sig
    type t
    val unstring : t Unstring.t
    type foo
    val unstring_foo : foo Unstring.t
    type 'a wrapper
    val unstring_wrapper : 'a Unstring.t -> 'a wrapper Unstring.t
  end
```

### Deriving `of_string`

In addition to the parser, derive an `of_string` function by passing `~of_string` and `~on_ambiguity`:

```ocaml
type direction =
  | North [@format "N"]
  | South [@format "S"]
[@@deriving sexp_of, unstring ~of_string ~on_ambiguity:`Raise]
```

```ocaml
# direction_of_string "N"
- : direction = North
```

`~on_ambiguity` controls behavior when multiple parses exist:

| Value | Behavior |
|-------|----------|
| `` `Raise `` | Raises with a sexp of the ambiguous value. Requires `sexp_of` derived **before** `unstring`. |
| `` `Raise_opaque `` | Raises with an opaque sexp (no `sexp_of` dependency). |
| `` `Pick_first `` | Silently picks the first valid parse. |

For non-`t` type names, the generated binding is `<name>_of_string`.

`~of_string` is not supported for parameterized types. Use `Unstring.make_of_string` directly if needed.

### Deriving `restring`

Derive a `restring` function (the inverse of `unstring`) by passing `~restring`:

```ocaml
type direction =
  | North [@format "N"]
  | South [@format "S"]
[@@deriving unstring ~restring]
```

```ocaml
# restring_direction North
- : string = "N"
```

This can be combined with `~of_string`:

```ocaml
type direction =
  | North [@format "N"]
  | South [@format "S"]
[@@deriving sexp_of, unstring ~of_string ~on_ambiguity:`Raise ~restring]
```

For fields using `[@custom]` to override the parser, provide a corresponding
`[@custom_restring]` to override the restring function:

<!-- $MDX skip -->
```ocaml
type t = { flag : (bool [@custom my_bool_parser] [@custom_restring my_bool_restring]); label : string }
[@@deriving unstring ~restring] [@@format "%{flag}:%{label}"]
```

For non-`t` type names, the generated binding is `restring_<name>` (following the same
convention as `unstring_<name>`).

In signatures, `~restring` generates the appropriate `val` declaration:

```ocaml
module type S = sig
  type t [@@deriving unstring ~restring]
end
```

expands to include `val restring : t -> string`.

Note: why not use `to_string`? `to_string` has very specific semantics, but restring
mirrors the structure of unstring. For example, `to_string` for the unit type would have
the signature `unit -> string`, but for `restring`, it's `unit:string -> unit -> string`.

## Inspecting parser shapes

`Unstring.print_shape` renders a compact, regex-like representation of the parser's grammar. This is useful for debugging and snapshot testing:

```ocaml
# Unstring.print_shape [%unstring ("x=%{_}" : int)]
x= «(- ‖ + ‖ "") ⟦\d⟧⋆ ⟦\d⟧»
- : unit = ()
# Unstring.print_shape [%unstring: [ `On [@format "on"] | `Off [@format "off"] ]]
on ‖ off
- : unit = ()
```

## Attributes summary

| Attribute | Context | Purpose |
|-----------|---------|---------|
| `[@format "..."]` | Variant constructors, polymorphic variant rows, tuples | Format string for this case |
| `[@@format "..."]` | Type declarations (records) | Format string for the whole type |
| `[@unit "..."]` | Unit types | Expected literal for unit |
| `[@some "..."]` | Option types | Format for the Some case |
| `[@none "..."]` | Option types | Format for the None case |
| `[@sep "..."]` | List types | Separator between elements |
| `[@chars expr]` | String types | Restrict allowed characters via `Char_filter.t` |
| `[@custom expr]` | Any type | Override with a custom parser |
| `[@custom_restring expr]` | Any type | Override with a custom restring function |

## Naming conventions

| Type name | Generated `unstring` | Generated `of_string` | Generated `restring` |
|-----------|----------------------|-----------------------|----------------------|
| `t`       | `unstring`           | `of_string`           | `restring`           |
| `foo`     | `unstring_foo`       | `foo_of_string`       | `restring_foo`       |

`unstring` and `restring` follow the same naming convention (prefix). `of_string` follows
the OCaml convention (suffix).
