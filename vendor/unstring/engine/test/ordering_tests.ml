open! Core
module Token = Common.Token
module Grammar = Common.Grammar

(* The point of this file is to test that the parse sequence follows a stable, formally
   describable ordering. This is important for backwards compatibility: even if we change
   the engine implementation, we do not want to accidentally break the client code by
   changing the parse ordering.

   The ordering is "right-lexicographic" to a first approximation. That is, suppose that
   we recorded indices for each grammar node during parsing, such that we get a list at
   the end. We then expect the parse results to be ordered lexicographically, if read from
   the right.

   For example, if we have parses that recorded indices [1;2;3] and [1;2;4], the first
   comes before the second because reading from the right, 3 < 4.

   In practice it's a bit more complicated than that; see the [Ordering_record] module for
   more details.

   The approach:
   1. Define [Ordering_record.t] — the type we compare to check ordering
   2. Define [Blueprint.t] — a simplified grammar for test generation (like [Blueprint.t]
      in semantics_tests.ml, but only the constructors that affect parse ordering: no Map,
      Capture, etc.)
   3. Convert blueprints to grammars that track indices via [Ordering_record.t]
   4. Generate random blueprints and inputs, verify parses are sorted

   We filter out cases with 0 or 1 parses in the generator, since those don't actually
   test ordering. *)

(* [Ordering_record] captures the indices recorded during a parse. Each grammar node is
   assigned a unique index. During parsing, we push entries onto a flat list
   ([Builder.t]): [`Token i] when consuming the token at node [i], [`Choice i] when making
   a choice (union branch or star iteration) at node [i].

   For comparison, [grouped] restructures the flat list into [t]: a list of entries, each
   pairing a token with the choice indices that followed it. This grouping is what gives
   us "right-lexicographic" ordering: we compare entry-by-entry right-to-left, and within
   each entry we first compare the token index, and then the choice indices. *)
module Ordering_record = struct
  module Builder = struct
    type t = [ `Token of int | `Choice of int ] list [@@deriving compare, sexp_of]
  end

  module Entry = struct
    type t =
      { token : int option
      ; choices : int list
      }
    [@@deriving compare, sexp_of]
  end

  type t = Entry.t Nonempty_list.t [@@deriving compare, sexp_of]

  let rec of_builder : Builder.t -> t = function
    | [] -> [ { token = None; choices = [] } ]
    | `Token t :: ts ->
      let ts = of_builder ts in
      Nonempty_list.cons { Entry.token = Some t; choices = [] } ts
    | `Choice c :: ts ->
      let (entry :: rest) = of_builder ts in
      { entry with choices = c :: entry.choices } :: rest
  ;;
end

module Blueprint = struct
  type t =
    | Return
    | Terminal of Token.t
    | Union of t list
    | Append of t * t
    | Star of t
  [@@deriving quickcheck, sexp_of]

  let rec of_grammar : type a. a Grammar.t -> t = function
    | Return _ -> Return
    | Map _ -> failwith "Map does not affect ordering"
    | Terminal t -> Terminal t
    | Capture _ | Capture_also _ -> failwith "Capture does not affect ordering"
    | Union ts -> Union (List.map ts ~f:of_grammar)
    | Append (tl, tr, _) -> Append (of_grammar tl, of_grammar tr)
    | Concat_unit _ -> failwith "Concat_unit is equivalent to folded Append"
    | Star (t, _) -> Star (of_grammar t)
    | Star_unit _ -> failwith "Star_unit is equivalent to Star"
  ;;

  let _of_grammar_is_only_used_for_exhaustiveness_check = of_grammar

  let rec to_ordering_grammar ~base : t -> Ordering_record.Builder.t Grammar.t * base:int
    = function
    | Return -> Return [], ~base
    | Terminal t -> Map (Terminal t, fun () -> [ `Token base ]), ~base:(base + 1)
    | Union ts ->
      let base, ts =
        List.fold_map ts ~init:base ~f:(fun base t ->
          let t, ~base = to_ordering_grammar ~base t in
          let choice_index = base in
          base + 1, Grammar.Map (t, fun stack -> `Choice choice_index :: stack))
      in
      Union ts, ~base
    | Append (ta, tb) ->
      let ta, ~base = to_ordering_grammar ~base ta in
      let tb, ~base = to_ordering_grammar ~base tb in
      Append (ta, tb, fun l r -> r @ l), ~base
    | Star t ->
      let t, ~base = to_ordering_grammar ~base t in
      let init, ~base = [ `Choice base ], ~base:(base + 1) in
      Star (t, { init; f = (fun acc a -> `Choice base :: (a @ acc)) }), ~base:(base + 1)
  ;;

  let to_ordering_grammar t =
    let grammar, ~base:_ = to_ordering_grammar ~base:0 t in
    Grammar.Map (grammar, Ordering_record.of_builder)
  ;;
end

(* These demonstrate the expected ordering for specific grammars and inputs. The first
   group shows the [Ordering_record.t] values produced by [Blueprint.to_ordering_grammar],
   making the ordering structure concrete. The second group uses [Capture] to show the
   same orderings in a more human-readable form (segment lengths). *)

let parse_ordering_records blueprint tokens =
  let grammar = Blueprint.to_ordering_grammar blueprint in
  Common.parse grammar tokens |> Sequence.to_list
;;

let%expect_test "ordering records: concat of star" =
  (* Concat (Star A, Star A) parsing [A; A]: two ways to split the tokens. Each record
     shows: one entry per token consumed, plus trailing choices for star-empty decisions.
     The records are sorted right-lexicographically. *)
  let blueprint = Blueprint.Append (Star (Terminal A), Star (Terminal A)) in
  parse_ordering_records blueprint [ A; A ]
  |> [%sexp_of: Ordering_record.t list]
  |> print_s;
  [%expect
    {|
    ((((token (0)) (choices (4 2))) ((token (0)) (choices (2)))
      ((token ()) (choices (1))))
     (((token (3)) (choices (5))) ((token (0)) (choices (4 2)))
      ((token ()) (choices (1))))
     (((token (3)) (choices (5))) ((token (3)) (choices (5)))
      ((token ()) (choices (4 1)))))
    |}]
;;

let%expect_test "ordering records: union" =
  (* Union of three epsilon branches: no tokens, just choice indices distinguishing which
     branch was taken. *)
  let blueprint = Blueprint.Union [ Return; Return; Return ] in
  parse_ordering_records blueprint [] |> [%sexp_of: Ordering_record.t list] |> print_s;
  [%expect
    {|
    ((((token ()) (choices (0)))) (((token ()) (choices (1))))
     (((token ()) (choices (2)))))
    |}]
;;

let%expect_test "ordering records: star" =
  (* Star A parsing [A; A]: the record shows how each iteration is tracked. *)
  let blueprint = Blueprint.Star (Terminal A) in
  parse_ordering_records blueprint [ A; A ]
  |> [%sexp_of: Ordering_record.t list]
  |> print_s;
  [%expect
    {|
    ((((token (0)) (choices (2))) ((token (0)) (choices (2)))
      ((token ()) (choices (1)))))
    |}]
;;

(* The actual property test *)

module Ordering_test = struct
  type t =
    { grammar : Blueprint.t
    ; tokens : Token.t list
    ; outcome : unit Or_error.t
    }
  [@@deriving sexp_of]

  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%map grammar = [%quickcheck.generator: Blueprint.t] in
    let indexed_grammar = Blueprint.to_ordering_grammar grammar in
    let%map.Option token_gen = Common.matching_tokens_for_grammar indexed_grammar in
    let%map.Quickcheck.Generator tokens = token_gen in
    let results = Common.parse indexed_grammar tokens in
    let results = Sequence.take results 100 |> Sequence.to_list in
    let%map.Option outcome =
      match results with
      | [] | [ _ ] ->
        (* 0 or 1 results doesn't test ordering - filter out *)
        None
      | _ ->
        if List.is_sorted results ~compare:[%compare: Ordering_record.t]
        then Some (Ok ())
        else
          Some
            (Or_error.error_s
               [%message "Results not sorted" (results : Ordering_record.t list)])
    in
    { grammar; tokens; outcome }
  ;;

  let quickcheck_generator =
    generator
    |> Quickcheck.Generator.filter_map ~f:Fn.id
    |> Quickcheck.Generator.join
    |> Quickcheck.Generator.filter_map ~f:Fn.id
  ;;

  let (quickcheck_shrinker : t Quickcheck.Shrinker.t) = Quickcheck.Shrinker.empty ()
end

let%expect_test "parse ordering is right-lexicographic" =
  let%quick_test prop ({ outcome; _ } : Ordering_test.t) = Or_error.ok_exn outcome in
  [%expect {| |}]
;;

(* Concrete orderings as an example *)

let capture : (int, string) Unstring_engine.Capture.t = fun _ ~pos:_ ~len -> len

let%expect_test "union eps" =
  let (grammar : string Grammar.t) =
    Union [ Return "first"; Return "second"; Return "third" ]
  in
  Common.parse grammar [] |> Sequence.to_list |> [%sexp_of: string list] |> print_s;
  [%expect {| (first second third) |}]
;;

let%expect_test "union token" =
  let (grammar : string Grammar.t) =
    let make s : string Grammar.t = Map (Terminal A, fun () -> s) in
    Union [ make "first"; make "second"; make "third" ]
  in
  Common.parse grammar [ A ] |> Sequence.to_list |> [%sexp_of: string list] |> print_s;
  [%expect {| (first second third) |}]
;;

let%expect_test "concat of star" =
  let (grammar : int list Grammar.t) =
    Append
      ( Capture (Star_unit (Terminal A), capture)
      , Capture (Star_unit (Terminal A), capture)
      , fun l r -> [ l; r ] )
  in
  let print_parses input =
    Common.parse grammar input |> Sequence.to_list |> [%sexp_of: int list list] |> print_s
  in
  print_parses [ A ];
  print_parses [ A; A ];
  print_parses [ A; A; A ];
  [%expect
    {|
    ((1 0) (0 1))
    ((2 0) (1 1) (0 2))
    ((3 0) (2 1) (1 2) (0 3))
    |}]
;;

let%expect_test "star of plus" =
  let (grammar : int list Grammar.t) =
    Star
      ( Capture (Concat_unit [ Terminal A; Star_unit (Terminal A) ], capture)
      , { init = []; f = (fun acc a -> acc @ [ a ]) } )
  in
  let print_parses input =
    Common.parse grammar input |> Sequence.to_list |> [%sexp_of: int list list] |> print_s
  in
  print_parses [ A ];
  print_parses [ A; A ];
  print_parses [ A; A; A ];
  [%expect
    {|
    ((1))
    ((1 1) (2))
    ((1 1 1) (2 1) (1 2) (3))
    |}]
;;

let%expect_test "star of star" =
  let (grammar : int list Grammar.t) =
    Star
      ( Capture (Star_unit (Terminal A), capture)
      , { init = []; f = (fun acc a -> acc @ [ a ]) } )
  in
  let print_parses input =
    Sequence.take (Common.parse grammar input) 5
    |> Sequence.to_list
    |> [%sexp_of: int list list]
    |> print_s
  in
  print_parses [ A ];
  print_parses [ A; A ];
  print_parses [ A; A; A ];
  [%expect
    {|
    ((1) (0 1) (0 0 1) (0 0 0 1) (0 0 0 0 1))
    ((2) (0 2) (0 0 2) (0 0 0 2) (0 0 0 0 2))
    ((3) (0 3) (0 0 3) (0 0 0 3) (0 0 0 0 3))
    |}]
;;

let%expect_test "posix vs perl" =
  let (grammar : int list Grammar.t) =
    Append
      ( Capture (Union [ Terminal A; Concat_unit [ Terminal A; Terminal A ] ], capture)
      , Capture (Union [ Concat_unit [ Terminal A; Terminal A ]; Terminal A ], capture)
      , fun l r -> [ l; r ] )
  in
  let print_parses input =
    Common.parse grammar input |> Sequence.to_list |> [%sexp_of: int list list] |> print_s
  in
  print_parses [ A ];
  print_parses [ A; A ];
  print_parses [ A; A; A ];
  [%expect
    {|
    ()
    ((1 1))
    ((1 2) (2 1))
    |}]
;;
