open! Core

module Char_filter = struct
  type t = Unstring.Char_filter.t =
    | Any
    | Digit
    | Alpha
    | Whitespace
    | Char of char
    | Fun of (char -> bool)
    | Not of t
  [@@deriving quickcheck, sexp]

  let char_gen t =
    let matching_chars = List.filter Char.all ~f:(Unstring.Char_filter.matches t) in
    match matching_chars with
    | [] -> None
    | _ -> Some (Quickcheck.Generator.of_list matching_chars)
  ;;
end

module Bound = struct
  let p = 0.7

  module Repr = struct
    type t =
      | Incl of (int[@generator Quickcheck.Generator.geometric 0 ~p]) [@weight 1.]
      | Excl of (int[@generator Quickcheck.Generator.geometric 0 ~p]) [@weight 1.]
      | Unbounded [@weight 6.]
    [@@deriving quickcheck, sexp]
  end

  type t = Unstring.Bound.t

  let to_repr : t -> Repr.t = function
    | Incl i -> Incl i
    | Excl i -> Excl i
    | Unbounded -> Unbounded
  ;;

  let of_repr : Repr.t -> t = function
    | Incl i -> Incl i
    | Excl i -> Excl i
    | Unbounded -> Unbounded
  ;;

  let to_quickcheckable, of_quickcheckable = to_repr, of_repr
  let to_sexpable, of_sexpable = to_repr, of_repr

  include functor Sexpable.Of_sexpable (Repr)
  include functor Quickcheckable.Of_quickcheckable (Repr)

  let normalize_l = function
    | Incl x -> x
    | Excl x -> x + 1
    | Unbounded -> 0
  ;;

  let normalize_r = function
    | Incl x -> Some x
    | Excl x -> Some (x - 1)
    | Unbounded -> None
  ;;

  let count_gen (l, r) =
    match normalize_l l, normalize_r r with
    | l, Some r ->
      if l <= r
      then
        Quickcheck.Generator.geometric l ~p
        |> Quickcheck.Generator.filter ~f:(fun i -> i <= r)
        |> Some
      else None
    | l, None -> Some (Quickcheck.Generator.geometric l ~p)
  ;;
end

module Parse = struct
  type t =
    | Unit
    | Return
    | Map of t
    | Map2 of t * t
    | Capture of string
    | List of t list
    | Fold of t list
  [@@deriving compare, sexp_of, variants]

  include functor Comparable.Make_plain
end

(* Attaching extra weight to leaf nodes, so that quickcheck tests get generated faster *)
type t =
  | Return [@weight 3.]
  | Map of t
  | Map2 of t * t
  | Char of Char_filter.t [@weight 3.]
  | Lit of string [@weight 3.]
  | Star of t
  | Multiple of
      { sep : t option
      ; bounds : (Bound.t * Bound.t) option
      ; item : t
      }
  | Capture of t
  | List_ of
      { sep : t option
      ; bounds : (Bound.t * Bound.t) option
      ; item : t
      }
  | Fold of
      { sep : t option
      ; bounds : (Bound.t * Bound.t) option
      ; item : t
      }
  | Choice of t list
  | Map_hlist of t list
[@@deriving quickcheck, sexp]

let rec to_unstring : t -> Parse.t Unstring.t =
  let unit t = Unstring.map (to_unit_unstring t) ~f:(fun () -> Parse.unit) in
  function
  | Return -> Unstring.return Parse.return
  | Map t -> Unstring.map (to_unstring t) ~f:Parse.map
  | Map2 (a, b) -> Unstring.map2 (to_unstring a) (to_unstring b) ~f:Parse.map2
  | (Char _ | Lit _ | Star _ | Multiple _) as t -> unit t
  | Capture t -> Unstring.capture (to_unit_unstring t) |> Unstring.map ~f:Parse.capture
  | List_ { sep; bounds; item } ->
    Unstring.list ?sep:(Option.map sep ~f:to_unit_unstring) ?bounds (to_unstring item)
    |> Unstring.map ~f:Parse.list
  | Fold { sep; bounds; item } ->
    Unstring.fold
      ?sep:(Option.map sep ~f:to_unit_unstring)
      ?bounds
      (to_unstring item)
      ~init:[]
      ~f:(fun acc x -> x :: acc)
    |> Unstring.map ~f:(fun xs -> List.rev xs |> Parse.fold)
  | Choice ts -> Unstring.choice (List.map ts ~f:to_unstring)
  | Map_hlist ts ->
    let module Acc = struct
      type t =
        | T : 'shape Unstring.Hlist.t * ('shape Heterogeneous_list.t -> Parse.t list) -> t
    end
    in
    let rec loop : t list -> Acc.t = function
      | [] -> T ([], fun [] -> [])
      | t :: ts ->
        let t = to_unstring t in
        let (T (args, f)) = loop ts in
        T (t :: args, fun (t :: args) -> t :: f args)
    in
    let (T (args, f)) = loop ts in
    Unstring.map_hlist args ~f:(fun args -> f args |> Parse.list)

and to_unit_unstring : t -> unit Unstring.t =
  let default t = Unstring.map (to_unstring t) ~f:(fun (_ : Parse.t) -> ()) in
  function
  | Return -> Unstring.return ()
  | Map t -> Unstring.map (to_unit_unstring t) ~f:(fun () -> ())
  | Map2 (a, b) ->
    Unstring.map2 (to_unit_unstring a) (to_unit_unstring b) ~f:(fun () () -> ())
  | Char cf -> Unstring.char cf
  | Lit s -> Unstring.lit s
  | Star t -> Unstring.star (to_unit_unstring t)
  | Multiple { sep; bounds; item } ->
    Unstring.multiple
      ?sep:(Option.map sep ~f:to_unit_unstring)
      ?bounds
      (to_unit_unstring item)
  | t -> default t
;;

let rec repeat_gen ~sep ~bounds item_gen =
  let%map.Option item_gen
  and n = Bound.count_gen (Option.value bounds ~default:(Unbounded, Unbounded))
  and sep_gen =
    match sep with
    | Some sep_bp -> matching_string_gen sep_bp
    | None -> Some String_gen.empty
  in
  String_gen.repeat ~sep:sep_gen ~n item_gen

and matching_string_gen : t -> String_gen.t option = function
  | Return -> Some String_gen.empty
  | Map t -> matching_string_gen t
  | Map2 (a, b) ->
    let%map.Option ga = matching_string_gen a
    and gb = matching_string_gen b in
    String_gen.concat ga gb
  | Char cf ->
    let%map.Option c = Char_filter.char_gen cf in
    String_gen.of_char c
  | Lit s -> Some (String_gen.literal s)
  | Star t ->
    let%map.Option g = matching_string_gen t in
    String_gen.star g
  | Capture t -> matching_string_gen t
  | Multiple { sep; bounds; item } -> repeat_gen ~sep ~bounds (matching_string_gen item)
  | List_ { sep; bounds; item } -> repeat_gen ~sep ~bounds (matching_string_gen item)
  | Fold { sep; bounds; item } -> repeat_gen ~sep ~bounds (matching_string_gen item)
  | Choice ts ->
    let gs = List.filter_map ts ~f:matching_string_gen in
    if List.is_empty gs then None else Some (String_gen.choice gs)
  | Map_hlist ts ->
    let%map.Option ts = List.map ts ~f:matching_string_gen |> Option.all in
    String_gen.concat_list ts
;;

let repeat_matching_string_gen ~sep ~bounds bp =
  repeat_gen ~sep ~bounds (matching_string_gen bp)
;;
