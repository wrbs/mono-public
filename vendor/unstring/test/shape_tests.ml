open! Core

(* This file features tests relating to [Unstring.Printable_shape]; both tests
   demonstrating the printer itself, but also tests freezing the snapshots of the common
   parsers in place.
*)

let print_shape t =
  t |> Unstring.to_printable_shape |> Unstring.Printable_shape.to_string |> print_endline
;;

(* ---- Tests for the printer itself ------------------------------------------------- *)

let%expect_test "terminal rendering" =
  let open Unstring in
  print_shape (char Any);
  [%expect {| · |}];
  print_shape (char Digit);
  [%expect {| ⟦\d⟧ |}];
  print_shape (char Alpha);
  [%expect {| ⟦\a⟧ |}];
  print_shape (char Whitespace);
  [%expect {| ⟦\s⟧ |}];
  print_shape (char (Char 'x'));
  [%expect {| x |}];
  print_shape (char (Not Digit));
  [%expect {| ⟦^\d⟧ |}];
  print_shape (char (Not (Not Alpha)));
  [%expect {| ⟦^^\a⟧ |}];
  print_shape (char (Fun (fun c -> Char.is_uppercase c || Char.is_digit c)));
  [%expect {| ⟦0-9A-Z⟧ |}]
;;

let%expect_test "empty and singleton unions" =
  let open Unstring in
  print_shape (choice []);
  [%expect {| ∅ |}];
  print_shape (choice [ lit "only" ]);
  [%expect {| only |}]
;;

let%expect_test "literal merging" =
  let open Unstring in
  (* Adjacent single-char terminals and literals are merged. *)
  print_shape (char (Char 'a') *> char (Char 'b') *> char (Char 'c'));
  [%expect {| abc |}];
  (* Characters that need quoting are rendered with sexp conventions. *)
  print_shape (lit "hello world");
  [%expect {| "hello world" |}]
;;

let%expect_test "composite grammar" =
  let open Unstring in
  let digits = multiple ~bounds:(Incl 1, Unbounded) (char Digit) in
  let timestamp = lit "(" *> capture digits <* lit ")" in
  let severity = choice [ lit "INFO"; lit "WARN"; lit "ERROR" ] in
  let message = string () in
  let line = both (both timestamp (lit " [" *> severity <* lit "] ")) message in
  print_shape line;
  [%expect {| "(" «⟦\d⟧⋆ ⟦\d⟧» ") [" (INFO ‖ WARN ‖ ERROR) "] " «·⋆» |}]
;;

let%expect_test "narrow width" =
  let open Unstring in
  let one_or_more t = multiple ~bounds:(Incl 1, Unbounded) t in
  let key = capture (one_or_more (char Alpha)) in
  let spaces = one_or_more (char Whitespace) in
  let value = capture (one_or_more (char (Not Whitespace))) in
  let values = list ~sep:(char (Char ';')) value in
  let comment = option (lit "#" *> string ()) ~none:(lit "") in
  let line =
    both
      (key <* spaces <* lit "=" <* spaces)
      (both values (star (char Whitespace) *> comment))
  in
  let shape = Unstring.to_printable_shape line in
  let fmt = Stdlib.Format.str_formatter in
  Stdlib.Format.pp_set_margin fmt 40;
  Unstring.Printable_shape.pp fmt shape;
  print_endline (Stdlib.Format.flush_str_formatter ());
  [%expect
    {|
    «⟦\a⟧⋆ ⟦\a⟧»
    ⟦\s⟧⋆
    ⟦\s⟧
    =
    ⟦\s⟧⋆
    ⟦\s⟧
    ((«⟦^\s⟧⋆ ⟦^\s⟧» ";")⋆ «⟦^\s⟧⋆ ⟦^\s⟧»
     ‖ "")
    ⟦\s⟧⋆
    (# «·⋆» ‖ "")
    |}]
;;

let%expect_test "glossary" =
  print_string Unstring.Printable_shape.glossary;
  [%expect
    {|
    Terminals
      abc     literal string (quoted if it contains special characters)
      ·       any character
      ⟦\d⟧    digit [0-9]
      ⟦\a⟧    alphabetic [A-Za-z]
      ⟦\s⟧    whitespace
      ⟦...⟧   character class
      ⟦^...⟧  negated character class

    Structure
      ""      epsilon (empty string / unit value)
      ∅       empty language (no match)
      ‖       alternation
      ⋆       zero-or-more (Kleene star, postfix)

    Grouping
      ( )     precedence grouping
      « »     capture group
    |}]
;;

(* ---- Shape snapshots for built-in parsers ----------------------------------------- *)

let%expect_test "multiple shape" =
  let x = Unstring.char (Char 'x') in
  let sep = Unstring.char (Char ',') in
  (* No bounds *)
  print_shape (Unstring.multiple ~bounds:(Unbounded, Unbounded) x);
  [%expect {| x⋆ |}];
  (* Upper bound only *)
  print_shape (Unstring.multiple ~bounds:(Unbounded, Incl 4) x);
  [%expect {| (((x ‖ "") x ‖ "") x ‖ "") x ‖ "" |}];
  (* Lower bound only *)
  print_shape (Unstring.multiple ~bounds:(Incl 2, Unbounded) x);
  [%expect {| x⋆ xx |}];
  (* Both bounds *)
  print_shape (Unstring.multiple ~bounds:(Incl 2, Incl 4) x);
  [%expect {| ((x ‖ "") x ‖ "") xx |}];
  (* With separator *)
  print_shape (Unstring.multiple ~sep ~bounds:(Unbounded, Unbounded) x);
  [%expect {| (x,)⋆ x ‖ "" |}];
  print_shape (Unstring.multiple ~sep ~bounds:(Incl 2, Unbounded) x);
  [%expect {| (x,)⋆ x,x |}];
  print_shape (Unstring.multiple ~sep ~bounds:(Incl 2, Incl 4) x);
  [%expect {| ((x, ‖ "") x, ‖ "") x,x |}];
  (* Impossible bounds *)
  print_shape (Unstring.multiple ~bounds:(Incl 3, Incl 1) x);
  [%expect {| ∅ |}];
  (* Exactly one *)
  print_shape (Unstring.multiple ~bounds:(Incl 1, Incl 1) x);
  [%expect {| x |}];
  (* Exactly zero *)
  print_shape (Unstring.multiple ~bounds:(Incl 0, Incl 0) x);
  [%expect {| "" |}]
;;

let%expect_test "list shape" =
  let x = Unstring.char (Char 'x') in
  let sep = Unstring.char (Char ',') in
  print_shape (Unstring.list x);
  [%expect {| x⋆ |}];
  print_shape (Unstring.list ~sep x);
  [%expect {| (x,)⋆ x ‖ "" |}]
;;

let%expect_test "bool shape" =
  print_shape Unstring.bool;
  [%expect {| false ‖ true |}]
;;

let%expect_test "int shape" =
  print_shape Unstring.int;
  [%expect {| «(- ‖ + ‖ "") ⟦\d⟧⋆ ⟦\d⟧» |}]
;;

let%expect_test "float shape" =
  print_shape Unstring.float;
  [%expect
    {|
    «(- ‖ + ‖ "")
     (⟦.0-9⟧⋆ ⟦.0-9⟧ ("" ‖ ⟦Ee⟧ (- ‖ + ‖ "") ⟦\d⟧⋆ ⟦\d⟧) ‖ nan ‖ inf)»
    |}]
;;

let%expect_test "string shape" =
  print_shape (Unstring.string ());
  [%expect {| «·⋆» |}];
  print_shape (Unstring.string ~chars:(Char 'x') ());
  [%expect {| «x⋆» |}]
;;
