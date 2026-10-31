open! Core

type t =
  | Literal of string
  | Terminal of Char_filter.t
  | Capture of t
  | Union of t list
  | Concat of t list
  | Star of t
[@@deriving sexp_of]

let rec of_grammar : type a. a Defs.Grammar.t -> t = function
  | Return _ -> Literal ""
  | Map (t, _) -> of_grammar t
  | Terminal t -> Terminal t
  | Capture (t, _) -> Capture (of_grammar t)
  | Capture_also (t, _) -> Capture (of_grammar t)
  | Union ts -> Union (List.map ts ~f:of_grammar)
  | Append (tl, tr, _) -> Concat [ of_grammar tl; of_grammar tr ]
  | Concat_unit ts -> Concat (List.map ts ~f:of_grammar)
  | Star (t, _) -> Star (of_grammar t)
  | Star_unit t -> Star (of_grammar t)
;;

(* Flatten nested [Concat]s and merge adjacent [Terminal (Char c)] nodes into [Literal]s. *)
let normalize t =
  let rec flatten_concat (t : t) : t list =
    match t with
    | Concat ts -> List.concat_map ts ~f:flatten_concat
    | t -> [ normalize_one t ]
  and normalize_one (t : t) : t =
    match t with
    | Literal _ -> t
    | Terminal _ -> t
    | Capture t -> Capture (normalize_one t)
    | Union ts -> Union (List.map ts ~f:normalize_one)
    | Star t -> Star (normalize_one t)
    | Concat _ ->
      let flat = flatten_concat t in
      let merged = merge_literals flat in
      (match merged with
       | [] -> Literal ""
       | [ t ] -> t
       | ts -> Concat ts)
  and merge_literals (ts : t list) : t list =
    let flush_chars acc_chars acc =
      match List.rev acc_chars with
      | [] -> acc
      | chars -> Literal (String.of_char_list chars) :: acc
    in
    let rec go acc_chars acc (ts : t list) =
      match ts with
      | [] -> List.rev (flush_chars acc_chars acc)
      | Terminal (Char c) :: rest -> go (c :: acc_chars) acc rest
      | Literal s :: rest -> go (List.rev_append (String.to_list s) acc_chars) acc rest
      | t :: rest -> go [] (normalize_one t :: flush_chars acc_chars acc) rest
    in
    go [] [] ts
  in
  normalize_one t
;;

let of_grammar g = of_grammar g |> normalize

(* [render_char] is used inside ⟦⟧ character class brackets, where parens and whitespace
   have no special meaning. *)
let render_char c =
  if Char.is_print c
  then String.of_char c
  else (
    match c with
    | '\t' -> "\\t"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | _ -> Printf.sprintf "\\x%02x" (Char.to_int c))
;;

let render_literal s = Sexp.to_string_mach (Sexp.Atom s)

let render_char_class_body f =
  let matching = List.filter Char.all ~f in
  let n = List.length matching in
  let negate = n > 128 in
  let chars = if negate then List.filter Char.all ~f:(fun c -> not (f c)) else matching in
  let ranges =
    let rec go acc first last = function
      | [] -> List.rev ((first, last) :: acc)
      | c :: rest ->
        if Char.to_int c = Char.to_int last + 1
        then go acc first c rest
        else go ((first, last) :: acc) c c rest
    in
    match chars with
    | [] -> []
    | c :: rest -> go [] c c rest
  in
  let body =
    List.map ranges ~f:(fun (first, last) ->
      if Char.equal first last
      then render_char first
      else if Char.to_int last = Char.to_int first + 1
      then render_char first ^ render_char last
      else render_char first ^ "-" ^ render_char last)
    |> String.concat
  in
  if negate then "^" ^ body else body
;;

(* Classify [Fun f] by cardinality: empty, universal, singleton, or general class. *)
let classify_fun f =
  let matching = List.filter Char.all ~f in
  match List.length matching with
  | 0 -> `Empty
  | 256 -> `Universal
  | _ ->
    (match matching with
     | [ c ] -> `Singleton c
     | _ -> `Class)
;;

(* Renders the inner content of a terminal, without ⟦⟧ brackets. *)
let rec render_terminal_inner : Char_filter.t -> string = function
  | Any -> "·"
  | Digit -> "\\d"
  | Alpha -> "\\a"
  | Whitespace -> "\\s"
  | Char c -> render_char c
  | Fun f ->
    (match classify_fun f with
     | `Empty -> "∅"
     | `Universal -> "·"
     | `Singleton c -> render_char c
     | `Class -> render_char_class_body f)
  | Not t -> "^" ^ render_terminal_inner t
;;

let render_terminal (t : Char_filter.t) =
  match t with
  | Any -> "·"
  | Char c -> render_literal (String.of_char c)
  | Fun f ->
    (match classify_fun f with
     | `Empty -> "∅"
     | `Universal -> "·"
     | `Singleton c -> render_literal (String.of_char c)
     | `Class -> "⟦" ^ render_char_class_body f ^ "⟧")
  | Digit | Alpha | Whitespace | Not _ -> "⟦" ^ render_terminal_inner t ^ "⟧"
;;

(* Precedence levels: 0 = union, 1 = concat, 2 = atom (incl. star, capture, terminal) *)

let pp fmt t =
  let open Stdlib.Format in
  let rec render ~context_prec (t : t) =
    let needs_wrap my_prec = my_prec < context_prec in
    match t with
    | Literal s -> pp_print_string fmt (render_literal s)
    | Terminal term -> pp_print_string fmt (render_terminal term)
    | Capture t ->
      pp_print_string fmt "«";
      render ~context_prec:0 t;
      pp_print_string fmt "»"
    | Star t ->
      let needs_star_parens =
        match t with
        | Literal s -> String.length s > 1
        | _ -> false
      in
      if needs_star_parens then pp_print_char fmt '(';
      render ~context_prec:2 t;
      if needs_star_parens then pp_print_char fmt ')';
      pp_print_string fmt "⋆"
    | Union [] -> pp_print_string fmt "∅"
    | Union [ t ] -> render ~context_prec t
    | Union ts ->
      let wrap = needs_wrap 0 in
      if wrap then pp_print_char fmt '(';
      pp_open_hvbox fmt 0;
      (* Concat binds tighter than union, so context_prec:1 suffices — a nested concat
         won't be parenthesized, matching standard grammar conventions. *)
      List.iteri ts ~f:(fun i t ->
        if i > 0
        then (
          pp_print_break fmt 1 0;
          pp_print_string fmt "‖ ");
        render ~context_prec:1 t);
      pp_close_box fmt ();
      if wrap then pp_print_char fmt ')'
    | Concat [] -> pp_print_string fmt (render_literal "")
    | Concat [ t ] -> render ~context_prec t
    | Concat ts ->
      let wrap = needs_wrap 1 in
      if wrap then pp_print_char fmt '(';
      pp_open_hvbox fmt 0;
      List.iteri ts ~f:(fun i t ->
        if i > 0 then pp_print_break fmt 1 0;
        render ~context_prec:1 t);
      pp_close_box fmt ();
      if wrap then pp_print_char fmt ')'
  in
  render ~context_prec:0 t
;;

let to_string t =
  let buf = Buffer.create 128 in
  let fmt = Stdlib.Format.formatter_of_buffer buf in
  pp fmt t;
  Stdlib.Format.pp_print_flush fmt ();
  Buffer.contents buf
;;

let glossary =
  {|Terminals
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
  « »     capture group|}
;;
