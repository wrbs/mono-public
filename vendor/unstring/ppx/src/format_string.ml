open Base
open! Ppxlib
open Ast_builder.Default

(* ============================================================ *)
(* Hole_key: identifies a hole in a format string *)
(* ============================================================ *)

module Hole_key = struct
  module T = struct
    type t =
      | Number of int
      | Label of string
    [@@deriving sexp_of, compare ~localize]
  end

  include T
  include Comparator.Make (T)

  let to_string = function
    | Number n -> Int.to_string n
    | Label s -> s
  ;;

  let to_string_hum ~total_count = function
    | Number 1 when total_count = 1 -> "_"
    | Number n -> Int.to_string n
    | Label s -> s
  ;;

  let var_name t = "__arg_" ^ to_string t

  let label = function
    | Number _ -> None
    | Label s -> Some s
  ;;
end

(* ============================================================ *)
(* Part: a parsed piece of a format string *)
(* ============================================================ *)

module Part = struct
  type t =
    | Literal of string
    | Hole of Hole_key.t
end

(* ============================================================ *)
(* Step 1: Parse the format string into parts *)
(* ============================================================ *)

type t =
  { text : string
  ; loc : Location.t
  }

let add_literal string ~start ~until ~acc =
  if start >= until
  then acc
  else `Literal (String.sub string ~pos:start ~len:(until - start)) :: acc
;;

let validate_field_name raw_content ~loc =
  let name = String.strip raw_content in
  if not (String.for_all name ~f:(fun c -> Char.is_alphanum c || Char.equal c '_'))
  then Location.Error.raise (Location.Error.createf ~loc "invalid field name: %S" name);
  name
;;

(* Find the next special sequence starting at [pos]: either [%%] (escape) or [%{]
   (interpolation). Returns [None] if no special sequence is found.

   A lone [%] (not followed by [%] or [{]) is treated as a literal character and does not
   need escaping. Only [%{] has special meaning; [%%] is the escape for a literal [%]
   before [{]. *)
let find_next_special string ~pos =
  let len = String.length string in
  let rec scan i =
    if i + 1 >= len
    then None
    else if Char.equal string.[i] '%'
    then (
      match string.[i + 1] with
      | '%' -> Some (`Escape, i)
      | '{' -> Some (`Interp, i)
      | _ -> scan (i + 1))
    else scan (i + 1)
  in
  scan pos
;;

let parse_interpolation string ~loc ~content_start =
  match String.substr_index string ~pos:content_start ~pattern:"}" with
  | None -> Location.raise_errorf ~loc "unterminated %%{"
  | Some close_pos ->
    let raw_content =
      String.sub string ~pos:content_start ~len:(close_pos - content_start)
    in
    let name = validate_field_name raw_content ~loc in
    close_pos + 1, name
;;

(** Parse the format string into a flat list of literals and raw (unclassified) hole
    names. Holes appear as [`Hole name] where [name] is the raw content between [%{] and
    [}]. *)
let rec parse_raw string ~loc ~pos ~acc =
  let len = String.length string in
  match find_next_special string ~pos with
  | None ->
    let acc = add_literal string ~start:pos ~until:len ~acc in
    List.rev acc
  | Some (`Escape, esc_pos) ->
    let acc = add_literal string ~start:pos ~until:esc_pos ~acc in
    let acc = `Literal "%" :: acc in
    parse_raw string ~loc ~pos:(esc_pos + 2) ~acc
  | Some (`Interp, interp_pos) ->
    let acc = add_literal string ~start:pos ~until:interp_pos ~acc in
    let next_pos, name =
      parse_interpolation string ~loc ~content_start:(interp_pos + 2)
    in
    let acc = `Hole name :: acc in
    parse_raw string ~loc ~pos:next_pos ~acc
;;

(** Classify raw parts into [Part.t]: literal strings pass through, and raw hole names are
    resolved to [Hole_key.t] values ([_] and bare numbers become [Number], everything else
    becomes [Label]). Validates numbering constraints (1-indexed, in range). *)
let classify raw_parts ~loc =
  let classified =
    List.map raw_parts ~f:(function
      | `Literal s -> `Literal s
      | `Hole "_" -> `Hole `Underscore
      | `Hole s ->
        (match Int.of_string_opt s with
         | Some n ->
           if n < 1
           then
             Location.raise_errorf
               ~loc
               "Numbered holes are 1-indexed and must be positive, got: %n"
               n
           else `Hole (`Numbered n)
         | None -> `Hole (`Named s)))
  in
  let unnamed_hole_count =
    List.count classified ~f:(function
      | `Hole (`Underscore | `Numbered _) -> true
      | `Literal _ | `Hole (`Named _) -> false)
  in
  List.map classified ~f:(function
    | `Literal s -> Part.Literal s
    | `Hole `Underscore ->
      if unnamed_hole_count > 1
      then
        Location.raise_errorf
          ~loc
          "Cannot use %%{_} if there's more than one unnamed hole, found %d such holes."
          unnamed_hole_count
      else Hole (Number 1)
    | `Hole (`Numbered n) ->
      if n > unnamed_hole_count
      then
        Location.raise_errorf
          ~loc
          "Used %d in a numbered hole, but there %s only %d numbered %s."
          n
          (if unnamed_hole_count = 1 then "is" else "are")
          unnamed_hole_count
          (if unnamed_hole_count = 1 then "hole" else "holes")
      else Hole (Number n)
    | `Hole (`Named name) -> Hole (Label name))
;;

(* Parse the format string into a list of [Part.t] with classified [Hole_key.t] values.
   Raises on parse errors and invalid hole names. *)
let parse { text; loc } = parse_raw text ~loc ~pos:0 ~acc:[] |> classify ~loc

(* ============================================================ *)
(* Step 2: Match parsers with holes *)
(* ============================================================ *)

(* Assign [Hole_key.t] values to each entry: [None] entries are numbered 1, 2, ... in list
   order; [Some name] entries become [Label name]. *)
let hole_keys_of_list parsers =
  List.folding_map parsers ~init:0 ~f:(fun numbered_count (label, expr) ->
    match label with
    | None ->
      let n = numbered_count + 1 in
      n, (Hole_key.Number n, expr)
    | Some name -> numbered_count, (Hole_key.Label name, expr))
;;

(* Build a map from [Hole_key.t] to [expression] by matching the caller's expressions
   against the holes that appear in the format string.

   Raises on duplicate holes, missing expressions, or extra expressions. *)
let validate_holes ~parts ~parser_map ~loc =
  let hole_key_set =
    let hole_keys =
      List.filter_map parts ~f:(function
        | Part.Hole key -> Some key
        | Literal _ -> None)
    in
    match List.find_all_dups hole_keys ~compare:Hole_key.compare with
    | [] -> Set.of_list (module Hole_key) hole_keys
    | _ :: _ as dups ->
      let dups = List.map dups ~f:Hole_key.to_string |> String.concat ~sep:", " in
      Location.raise_errorf
        ~loc
        "The following holes have been specified more than once: %s."
        dups
  in
  (* Validate: every parser has a corresponding hole in the format string *)
  Map.iter_keys parser_map ~f:(fun key ->
    if not (Set.mem hole_key_set key)
    then
      Location.raise_errorf
        ~loc
        "The format string is expected to contain the hole %%{%s} but no such hole found"
        (Hole_key.to_string_hum ~total_count:(Map.length parser_map) key));
  (* Validate: every hole in the format string has a corresponding parser *)
  Set.iter hole_key_set ~f:(fun key ->
    if not (Map.mem parser_map key)
    then
      Location.raise_errorf
        ~loc
        "The hole %%{%s} inside the format string does not correspond to any field"
        (Hole_key.to_string_hum ~total_count:(Set.length hole_key_set) key))
;;

(* ============================================================ *)
(* Step 3: Build the expression *)
(* ============================================================ *)

module Fields = struct
  type t =
    | None
    | Singleton of { parser : expression }
    | Many of
        { parsers : (string option * expression) list
        ; combine : (string option * expression) list -> expression
        }
end

let build_expression_none ~parts ~loc =
  let parser_exprs =
    List.map parts ~f:(function
      | Part.Literal text -> [%expr Unstring.lit [%e estring ~loc text]]
      | Hole _ -> failwith "ppx_unstring bug: no holes expected")
  in
  [%expr Unstring.all_unit [%e elist ~loc parser_exprs]]
;;

let build_expression_singleton ~parts ~parser ~loc =
  let var_name = Hole_key.var_name (Number 1) in
  let parser_exprs, patterns =
    List.map parts ~f:(function
      | Part.Literal text -> [%expr Unstring.lit [%e estring ~loc text]], [%pat? ()]
      | Hole _ -> parser, pvar ~loc var_name)
    |> List.unzip
  in
  match parser_exprs with
  | [ parser ] -> parser
  | _ ->
    [%expr
      Unstring.map_hlist [%e elist ~loc parser_exprs] ~f:(fun [%p plist ~loc patterns] ->
        [%e evar ~loc var_name])]
;;

let build_expression_many ~parts ~parser_map ~caller_parsers ~combine ~loc =
  let parser_exprs, patterns =
    List.map parts ~f:(function
      | Part.Literal text -> [%expr Unstring.lit [%e estring ~loc text]], [%pat? ()]
      | Hole key -> Map.find_exn parser_map key, pvar ~loc (Hole_key.var_name key))
    |> List.unzip
  in
  let hole_args =
    List.map caller_parsers ~f:(fun (key, _) ->
      Hole_key.label key, evar ~loc (Hole_key.var_name key))
  in
  [%expr
    Unstring.map_hlist [%e elist ~loc parser_exprs] ~f:(fun [%p plist ~loc patterns] ->
      [%e combine hole_args])]
;;

(* ============================================================ *)
(* build_unstring: public entry point *)
(* ============================================================ *)

let build_string ({ loc = string_loc; _ } as t) ~loc ~hole_string_exprs =
  let parts = parse t in
  let caller_exprs = hole_keys_of_list hole_string_exprs in
  let expr_map = Map.of_alist_exn (module Hole_key) caller_exprs in
  validate_holes ~parts ~parser_map:expr_map ~loc:string_loc;
  let string_parts =
    List.map parts ~f:(function
      | Part.Literal text -> estring ~loc text
      | Part.Hole key -> Map.find_exn expr_map key)
  in
  match string_parts with
  | [] -> estring ~loc ""
  | [ single ] -> single
  | _ -> [%expr String.concat ~sep:"" [%e elist ~loc string_parts]]
;;

let build_unstring ({ loc = string_loc; _ } as t) ~loc ~fields =
  let parts = parse t in
  match (fields : Fields.t) with
  | None ->
    validate_holes ~parts ~parser_map:(Map.empty (module Hole_key)) ~loc:string_loc;
    build_expression_none ~parts ~loc
  | Singleton { parser } ->
    let parser_map = Map.singleton (module Hole_key) (Number 1) parser in
    validate_holes ~parts ~parser_map ~loc:string_loc;
    build_expression_singleton ~parts ~parser ~loc
  | Many { parsers; combine } ->
    let caller_parsers = hole_keys_of_list parsers in
    let parser_map = Map.of_alist_exn (module Hole_key) caller_parsers in
    validate_holes ~parts ~parser_map ~loc:string_loc;
    build_expression_many ~parts ~parser_map ~caller_parsers ~combine ~loc
;;
