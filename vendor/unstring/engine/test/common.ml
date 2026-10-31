open! Core

(** A simple two-token grammar for testing. Tokens are A and B, terminals match exactly
    one token, and segments are strings. *)

module Token = struct
  type t =
    | A
    | B
  [@@deriving compare, enumerate, equal, quickcheck, sexp_of]
end

module Token_lookup = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving sexp_of]

  let create (f : Token.t -> _) = { a = f A; b = f B }

  let find { a; b } : Token.t -> _ = function
    | A -> a
    | B -> b
  ;;
end

module Grammar = struct
  type token = Token.t [@@deriving sexp_of]
  type terminal = Token.t [@@deriving sexp_of]
  type input = string [@@deriving sexp_of]

  let matches terminal token = Token.equal terminal token

  module Token_lookup = Token_lookup

  include Unstring_engine.Make_derived (struct
      type nonrec terminal = terminal [@@deriving sexp_of]
      type nonrec input = input [@@deriving sexp_of]
    end)
end

module Parser = Unstring_engine.Parser.Make (Grammar)

let tokens_to_string tokens =
  String.of_char_list
    (List.map tokens ~f:(function
      | Token.A -> 'a'
      | Token.B -> 'b'))
;;

let parse grammar tokens =
  let input = tokens_to_string tokens in
  let parser =
    List.fold tokens ~init:(Parser.create grammar) ~f:(fun parser token ->
      Parser.advance parser token)
  in
  Sequence.unfold ~init:(Parser.reconstruct parser ~input) ~f:(fun r ->
    Unstring_engine.Fast_sequence.next r)
  |> Sequence.map ~f:(fun thunk -> thunk ())
;;

let rec matching_tokens_for_grammar
  : type a. a Grammar.t -> Token.t list Quickcheck.Generator.t option
  =
  let concat ta tb =
    let%map.Option ta = matching_tokens_for_grammar ta
    and tb = matching_tokens_for_grammar tb in
    let%map.Quickcheck.Generator ta and tb in
    ta @ tb
  in
  let star t =
    let%map.Option t = matching_tokens_for_grammar t in
    Quickcheck.Generator.recursive_union
      [ Quickcheck.Generator.return [] ]
      ~f:(fun self ->
        [ (let%map.Quickcheck.Generator t and self in
           t @ self)
        ])
  in
  function
  | Return _ -> Some (Quickcheck.Generator.return [])
  | Map (t, _) -> matching_tokens_for_grammar t
  | Terminal t -> Some (Quickcheck.Generator.return [ t ])
  | Capture (t, _) -> matching_tokens_for_grammar t
  | Capture_also (t, _) -> matching_tokens_for_grammar t
  | Union ts ->
    let ts = List.filter_map ts ~f:matching_tokens_for_grammar in
    if List.is_empty ts
    then None
    else Quickcheck.Generator.of_list ts |> Quickcheck.Generator.join |> Some
  | Append (ta, tb, _) -> concat ta tb
  | Concat_unit ts ->
    let%map.Option gens = List.map ts ~f:matching_tokens_for_grammar |> Option.all in
    Quickcheck.Generator.all gens |> Quickcheck.Generator.map ~f:List.concat
  | Star (t, _) -> star t
  | Star_unit t -> star t
;;
