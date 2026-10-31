open! Core
module Token = Common.Token
module Grammar = Common.Grammar

(* The idea behind this file is to implement a quickcheck test for the semantics of each
   grammar constructor, as written down in src/grammar_module_types_intf.ml. However, even
   though the specification there is written down in 'relational' form, like
   {v
   g ⇓ ts ⇒ v
   v}
   it is difficult to test such a form comprehensively. Instead, we operate on the parse
   sequences themselves, defining what they should be compositionally.

   The strength of the semantics tested here is, pragmatically, somewhere in-between
   fully-ordered and fully-unordered. That is, we do test that e.g. wrapping a grammar in
   a [Map] should not change the parse order, but we make no specific assertion for
   [Append] or [Star] (except that the first parse is composed of first parses too);
   instead ordering_tests.ml establishes that the parse ordering is stable and formally
   describable.

   Generating such testcases is a bit of a challenge. First, the [Grammar.t] type is
   type-indexed and contains closures, making it difficult to write a fully general
   generator. Fortunately, the closures are only executed at the very end and can't affect
   parse structure. So the approach we take is a [Blueprint.t] type, which is like a
   [Grammar.t] but with the types and closures stripped; it can be easily generated. Such
   a [Blueprint.t] can then be converted to a [Parse.t Grammar.t] most of the time (unless
   it is necessitated to be a [unit Grammar.t]), where [Parse.t] is the most general parse
   tree type (kind of like a free monoid). As long as any [Grammar.t] has a [Blueprint.t],
   we have not lost generality yet.

   Second, verifying the parse sequence itself. We use intermediate types [Semantics.t]
   and [Value_stream.t] to help express the desired assertion for each constructor at a
   high level. Unfortunately the parse sequence can be infinite; so we try to verify as
   much as we can, returning [`Inconclusive] if it didn't feel like we tested things to an
   adequate degree. (Which is also why we put the validation code inside the generator).

   And as for the testcase generator, we make sure that it tests something by having the
   input stream generator follow the grammar most of the time, such that there is at least
   one match.

   Overall, (not counting auxiliary tests), this leads to only one quickcheck test, but a
   very powerful one, as the proposition it tests, if true, inductively implies the
   correctness of the implementation for any possible grammar and input.
*)

module Parse = struct
  (* Technically speaking this type could be even more general if it included unique
     indices based on origin, but this should be good enough too. *)
  type t =
    | Unit
    | Return
    | Map of t
    | Append of t * t
    | Star_empty
    | Star_cons of t * t
    | Capture of string
    | Capture_also of string * t
  [@@deriving compare, sexp_of, variants]

  include functor Comparable.Make_plain
end

module Blueprint = struct
  type t =
    | Return
    | Map of t
    | Terminal of Token.t
    | Capture of t
    | Capture_also of t
    | Union of t list
    | Append of t * t
    | Concat_unit of t list
    | Star of t
    | Star_unit of t
  [@@deriving compare, equal, quickcheck, sexp_of]

  (* warning: one of the ways we could lose coverage is if the mapping here doesn't
     describe a perfect correspondence. There is a quickcheck test below that checks that,
     but it wouldn't catch if e.g. [Blueprint.t] didn't have the [Star_unit] constructor,
     and we just mapped [Grammar.Star_unit] to [Blueprint.Star].
  *)
  let rec of_grammar : type a. a Grammar.t -> t = function
    | Return _ -> Return
    | Map (t, _) -> Map (of_grammar t)
    | Terminal t -> Terminal t
    | Capture (t, _) -> Capture (of_grammar t)
    | Capture_also (t, _) -> Capture_also (of_grammar t)
    | Union ts -> Union (List.map ts ~f:of_grammar)
    | Append (tl, tr, _) -> Append (of_grammar tl, of_grammar tr)
    | Concat_unit ts -> Concat_unit (List.map ts ~f:of_grammar)
    | Star (t, _) -> Star (of_grammar t)
    | Star_unit t -> Star_unit (of_grammar t)
  ;;

  let rec to_grammar_unit : t -> unit Grammar.t = function
    | Return -> Return ()
    | Map t -> Map (to_grammar_unit t, fun () -> ())
    | Terminal t -> Terminal t
    | Capture t -> Capture (to_grammar_unit t, fun _ ~pos:_ ~len:_ -> ())
    | Capture_also t -> Capture_also (to_grammar_unit t, fun _ ~pos:_ ~len:_ () -> ())
    | Union ts -> Union (List.map ts ~f:to_grammar_unit)
    | Append (tl, tr) -> Append (to_grammar_unit tl, to_grammar_unit tr, fun () () -> ())
    | Concat_unit ts -> Concat_unit (List.map ts ~f:to_grammar_unit)
    | Star t -> Star (to_grammar_unit t, { init = (); f = (fun () () -> ()) })
    | Star_unit t -> Star_unit (to_grammar_unit t)
  ;;

  (* Roundtrips with [of_grammar] (asserted below). It tries to construct a
     [Parse.t Grammar.t], unless it's forced to construct a [unit Grammar.t].
     Unfortunately because of that it's a bit complicated, and I don't feel like thinking
     about ways to simplify it, but the quickcheck below makes me confident that it is
     correct.
  *)
  let rec to_grammar : t -> [ `Unit of unit Grammar.t | `Full of Parse.t Grammar.t ]
    = function
    | Return -> `Full (Return Parse.return)
    | Map t ->
      (match to_grammar t with
       | `Unit g -> `Full (Map (g, fun () -> Parse.map Parse.unit))
       | `Full g -> `Full (Map (g, Parse.map)))
    | Terminal t -> `Unit (Terminal t)
    | Capture t ->
      `Full
        (Capture
           ( to_grammar_unit t
           , fun input ~pos ~len -> Parse.capture (String.sub input ~pos ~len) ))
    | Capture_also t ->
      (match to_grammar t with
       | `Unit g ->
         `Full
           (Capture_also
              ( g
              , fun input ~pos ~len () ->
                  Parse.capture_also (String.sub input ~pos ~len) Parse.unit ))
       | `Full g ->
         `Full
           (Capture_also
              ( g
              , fun input ~pos ~len v -> Parse.capture_also (String.sub input ~pos ~len) v
              )))
    | Union ts ->
      let gs = List.map ts ~f:to_grammar in
      (match
         List.map gs ~f:(function
           | `Full g -> Some g
           | `Unit _ -> None)
         |> Option.all
       with
       | Some gs -> `Full (Union gs)
       | None -> `Unit (Union (List.map ts ~f:to_grammar_unit)))
    | Append (tl, tr) ->
      `Full
        (match to_grammar tl, to_grammar tr with
         | `Unit gl, `Unit gr ->
           Append (gl, gr, fun () () -> Parse.append Parse.unit Parse.unit)
         | `Full gl, `Unit gr -> Append (gl, gr, fun v () -> Parse.append v Parse.unit)
         | `Unit gl, `Full gr -> Append (gl, gr, fun () v -> Parse.append Parse.unit v)
         | `Full gl, `Full gr -> Append (gl, gr, Parse.append))
    | Concat_unit ts -> `Unit (Concat_unit (List.map ts ~f:to_grammar_unit))
    | Star t ->
      `Full
        (match to_grammar t with
         | `Unit g ->
           Star
             ( g
             , { init = Parse.star_empty; f = (fun v () -> Parse.star_cons v Parse.unit) }
             )
         | `Full g -> Star (g, { init = Parse.star_empty; f = Parse.star_cons }))
    | Star_unit t -> `Unit (Star_unit (to_grammar_unit t))
  ;;
end

let%expect_test "blueprint unit roundtrip" =
  let%quick_test prop (original : Blueprint.t) =
    let roundtripped = Blueprint.to_grammar_unit original |> Blueprint.of_grammar in
    [%test_result: Blueprint.t] roundtripped ~expect:original
  in
  [%expect {| |}]
;;

let%expect_test "blueprint general roundtrip" =
  let%quick_test prop (original : Blueprint.t) =
    let roundtripped =
      match Blueprint.to_grammar original with
      | `Unit g -> Blueprint.of_grammar g
      | `Full g -> Blueprint.of_grammar g
    in
    [%test_result: Blueprint.t] roundtripped ~expect:original
  in
  [%expect {| |}]
;;

module Semantics = struct
  type 'a t =
    | Fixed : (Token.t list -> 'a list) -> 'a t (* The parse sequence is as returned *)
    | Mapped : 'a Grammar.t * (Token.t list -> 'a -> 'b) -> 'b t
      (* The parse sequence is the same as for the given grammar, but remapped *)
    | Union : 'a t list -> 'a t
      (* The parse sequence consists of the following parse sequences, interleaved *)
    | Partition : 'a Grammar.t * 'b Grammar.t * ('a -> 'b -> 'c) -> 'c t
  (* The parse is a union of the cartesian products for each partition, interleaved *)

  let id g = Mapped (g, fun _ v -> v)

  let of_grammar : type a. a Grammar.t -> a t = function
    | Return x ->
      Fixed
        (function
          | [] -> [ x ]
          | _ :: _ -> [])
    | Map (t, f) -> Mapped (t, fun _ v -> f v)
    | Terminal terminal ->
      Fixed
        (function
          | [ token ] -> if Grammar.matches terminal token then [ () ] else []
          | [] | _ :: _ :: _ -> [])
    | Capture_also (t, f) ->
      Mapped
        ( t
        , fun tokens v ->
            f (Common.tokens_to_string tokens) ~pos:0 ~len:(List.length tokens) v )
    | Capture (t, f) -> id (Capture_also (t, fun input ~pos ~len () -> f input ~pos ~len))
    | Union ts -> Union (List.map ts ~f:id)
    | Append (tl, tr, f) -> Partition (tl, tr, f)
    | Concat_unit [] -> id (Return ())
    | Concat_unit (t :: ts) -> id (Append (t, Concat_unit ts, fun () () -> ()))
    | Star (t, { init; f }) ->
      Union [ id (Return init); Partition (Star (t, { init; f }), t, f) ]
    | Star_unit t -> id (Star (t, { init = (); f = (fun () () -> ()) }))
  ;;

  (* A Sequence.Let_syntax.map2 that does not hang when one of the sequences is infinite
     and the other empty *)
  let sequence_map2 l r ~f =
    if Sequence.is_empty l || Sequence.is_empty r
    then Sequence.empty
    else Sequence.cartesian_product l r |> Sequence.map ~f:(fun (l, r) -> f l r)
  ;;

  let rec partitions : 'a list -> ('a list * 'a list) list = function
    | [] -> [ [], [] ]
    | x :: xs -> ([], x :: xs) :: List.map (partitions xs) ~f:(fun (l, r) -> x :: l, r)
  ;;

  let rec to_value_stream : type a. Token.t list -> a t -> a Value_stream.t =
    fun input -> function
    | Fixed f -> Simple (f input |> Sequence.of_list)
    | Mapped (g, f) -> Simple (Common.parse g input |> Sequence.map ~f:(f input))
    | Union ts ->
      Interleaved
        (List.concat_map ts ~f:(fun t ->
           match to_value_stream input t with
           | Simple t -> [ t ]
           | Interleaved ts -> ts))
    | Partition (gl, gr, f) ->
      Interleaved
        (List.map (partitions input) ~f:(fun (l, r) ->
           sequence_map2 (Common.parse gl l) (Common.parse gr r) ~f))
  ;;
end

module Testcase = struct
  type t =
    { blueprint : Blueprint.t
    ; input : Token.t list
    ; outcome : unit Or_error.t
    }
  [@@deriving sexp_of]

  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let%map blueprint = [%quickcheck.generator: Blueprint.t]
    and which_input =
      Quickcheck.Generator.weighted_union [ 0.8, return `Matching; 0.2, return `Any ]
    in
    let%bind.Option grammar =
      match Blueprint.to_grammar blueprint with
      | `Full g -> Some g
      | `Unit _ -> None
    in
    let%map.Option input_gen =
      match which_input with
      | `Matching -> Common.matching_tokens_for_grammar grammar
      | `Any -> Some [%quickcheck.generator: Token.t list]
    in
    let%map.Quickcheck.Generator input = input_gen in
    let expected = Semantics.of_grammar grammar |> Semantics.to_value_stream input in
    let actual = Common.parse grammar input in
    let%map.Option outcome =
      match Value_stream.test (module Parse) ~actual ~expected ~cutoff:100 with
      | `Ok -> Some (Ok ())
      | `Error err -> Some (Error err)
      | `Inconclusive -> None
    in
    { blueprint; input; outcome }
  ;;

  let quickcheck_generator =
    generator
    |> Quickcheck.Generator.filter_map ~f:Fn.id
    |> Quickcheck.Generator.join
    |> Quickcheck.Generator.filter_map ~f:Fn.id
  ;;

  let (quickcheck_shrinker : t Quickcheck.Shrinker.t) = Quickcheck.Shrinker.empty ()
end

let%expect_test "Semantics" =
  let%quick_test prop ({ outcome; _ } : Testcase.t) = Or_error.ok_exn outcome in
  [%expect {| |}]
;;
