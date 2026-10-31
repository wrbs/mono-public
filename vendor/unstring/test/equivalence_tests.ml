open! Core

(** Tests that equivalent constructions in [Unstring] produce the same parse results.

    Each test declares two functions [lhs] and [rhs] from some testcase type to
    ['a Unstring.t]. The scaffold generates random testcases, builds both parsers, and
    checks they agree on: matching inputs (likely to parse) and random inputs (likely to
    be rejected). *)

module Make (M : sig
    type testcase [@@deriving quickcheck, sexp_of]
    type result [@@deriving compare, sexp_of]

    val lhs : testcase -> result Unstring.t
    val rhs : testcase -> result Unstring.t
    val matching_string_gen : testcase -> string Quickcheck.Generator.t option
  end) : sig
  type t [@@deriving quickcheck, sexp_of]

  val validate : t -> unit
end = struct
  type t =
    { testcase : M.testcase
    ; inputs : ([ `Matching | `Any ] * string) list
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Generator.Let_syntax in
    let%bind testcase = [%quickcheck.generator: M.testcase] in
    let%map inputs =
      [ M.matching_string_gen testcase
        |> Option.map ~f:(Quickcheck.Generator.map ~f:(fun x -> `Matching, x))
      ; Some
          ([%quickcheck.generator: string]
           |> Quickcheck.Generator.map ~f:(fun x -> `Any, x))
      ]
      |> List.filter_opt
      |> Quickcheck.Generator.union
      |> Quickcheck.Generator.list_with_length 5
    in
    { testcase; inputs }
  ;;

  let quickcheck_observer = Quickcheck.Observer.singleton ()
  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

  let validate { testcase; inputs } =
    let lhs = M.lhs testcase |> Unstring.compile in
    let rhs = M.rhs testcase |> Unstring.compile in
    let run t input =
      Sequence.take (Unstring.Compiled.run_all_parses t input) 10 |> Sequence.to_list
    in
    List.iter inputs ~f:(fun (requirement, input) ->
      let lhs, rhs = run lhs input, run rhs input in
      [%test_eq: M.result Or_error.t list] lhs rhs;
      match requirement with
      | `Any -> ()
      | `Matching ->
        (* Double-checking that our inputs that we intend to match do indeed match *)
        assert (List.length lhs > 0))
  ;;
end

let%expect_test "map2 is associative" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t * Blueprint.t * Blueprint.t
      [@@deriving quickcheck, sexp_of]

      type result = Blueprint.Parse.t * Blueprint.Parse.t * Blueprint.Parse.t
      [@@deriving compare, sexp_of]

      let map2 = Unstring.map2
      let to_unstring t = Tuple3.map t ~f:Blueprint.to_unstring

      let lhs t =
        let a, b, c = to_unstring t in
        map2 (map2 a b ~f:(fun a b -> a, b)) c ~f:(fun (a, b) c -> a, b, c)
      ;;

      let rhs t =
        let a, b, c = to_unstring t in
        map2 a (map2 b c ~f:(fun b c -> b, c)) ~f:(fun a (b, c) -> a, b, c)
      ;;

      let matching_string_gen (a, b, c) =
        [ Blueprint.matching_string_gen a
        ; Blueprint.matching_string_gen b
        ; Blueprint.matching_string_gen c
        ]
        |> Option.all
        |> Option.map ~f:String_gen.concat_list
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "map2 left identity: map2 (return ()) t ≡ t" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = Blueprint.Parse.t [@@deriving compare, sexp_of]

      let lhs bp =
        Unstring.map2 (Unstring.return ()) (Blueprint.to_unstring bp) ~f:(fun () x -> x)
      ;;

      let rhs bp = Blueprint.to_unstring bp
      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "map2 right identity: map2 t (return ()) ≡ t" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = Blueprint.Parse.t [@@deriving compare, sexp_of]

      let lhs bp =
        Unstring.map2 (Blueprint.to_unstring bp) (Unstring.return ()) ~f:(fun x () -> x)
      ;;

      let rhs bp = Blueprint.to_unstring bp
      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "map2 naturality: map (map2 a b ~f) ~g ≡ map2 a b ~f:(compose g f)" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t * Blueprint.t [@@deriving quickcheck, sexp_of]

      type result = [ `G of [ `F of Blueprint.Parse.t * Blueprint.Parse.t ] ]
      [@@deriving compare, sexp_of]

      let f a b = `F (a, b)
      let g x = `G x

      let lhs (a, b) =
        Unstring.map2 (Blueprint.to_unstring a) (Blueprint.to_unstring b) ~f
        |> Unstring.map ~f:g
      ;;

      let rhs (a, b) =
        Unstring.map2 (Blueprint.to_unstring a) (Blueprint.to_unstring b) ~f:(fun a b ->
          g (f a b))
      ;;

      let matching_string_gen (a, b) =
        [ Blueprint.matching_string_gen a; Blueprint.matching_string_gen b ]
        |> Option.all
        |> Option.map ~f:String_gen.concat_list
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "functor identity: map t ~f:Fun.id ≡ t" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = Blueprint.Parse.t [@@deriving compare, sexp_of]

      let lhs bp = Unstring.map (Blueprint.to_unstring bp) ~f:Fun.id
      let rhs bp = Blueprint.to_unstring bp
      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "functor composition: map (map t ~f) ~g ≡ map t ~f:(compose g f)" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = [ `G of [ `F of Blueprint.Parse.t ] ] [@@deriving compare, sexp_of]

      let f x = `F x
      let g x = `G x
      let lhs bp = Blueprint.to_unstring bp |> Unstring.map ~f |> Unstring.map ~f:g
      let rhs bp = Blueprint.to_unstring bp |> Unstring.map ~f:(fun x -> g (f x))
      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "all_unit ts ≡ folding map2" =
  (* We should test that as [all_unit] follows a custom, more efficient implementation *)
  let module Test =
    Make (struct
      type testcase = Blueprint.t list [@@deriving quickcheck, sexp_of]
      type result = unit [@@deriving compare, sexp_of]

      let lhs bps = Unstring.all_unit (List.map bps ~f:Blueprint.to_unit_unstring)

      let rhs bps =
        List.fold
          (List.map bps ~f:Blueprint.to_unit_unstring)
          ~init:(Unstring.return ())
          ~f:(fun acc t -> Unstring.map2 acc t ~f:(fun () () -> ()))
      ;;

      let matching_string_gen bps =
        List.map bps ~f:Blueprint.matching_string_gen
        |> Option.all
        |> Option.map ~f:String_gen.concat_list
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "capture t ≡ capture' t ~f:String.sub" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = string [@@deriving compare, sexp_of]

      let lhs bp = Unstring.capture (Blueprint.to_unit_unstring bp)

      let rhs bp =
        Unstring.capture' (Blueprint.to_unit_unstring bp) ~f:(fun () ~input ~pos ~len ->
          String.sub input ~pos ~len)
      ;;

      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "star t ≡ multiple t" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = unit [@@deriving compare, sexp_of]

      let lhs bp = Unstring.star (Blueprint.to_unit_unstring bp)
      let rhs bp = Unstring.multiple (Blueprint.to_unit_unstring bp)

      let matching_string_gen bp =
        Blueprint.repeat_matching_string_gen ~sep:None ~bounds:None bp
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "star t ≡ fold t ~init:() ~f:unit" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = unit [@@deriving compare, sexp_of]

      let lhs bp = Unstring.star (Blueprint.to_unit_unstring bp)

      let rhs bp =
        Unstring.fold (Blueprint.to_unit_unstring bp) ~init:() ~f:(fun () () -> ())
      ;;

      let matching_string_gen bp =
        Blueprint.repeat_matching_string_gen ~sep:None ~bounds:None bp
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "multiple ~bounds t ≡ fold ~bounds t ~init:() ~f:unit" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t * Blueprint.Bound.t * Blueprint.Bound.t
      [@@deriving quickcheck, sexp_of]

      type result = unit [@@deriving compare, sexp_of]

      let lhs (bp, lo, hi) =
        Unstring.multiple ~bounds:(lo, hi) (Blueprint.to_unit_unstring bp)
      ;;

      let rhs (bp, lo, hi) =
        Unstring.fold
          ~bounds:(lo, hi)
          (Blueprint.to_unit_unstring bp)
          ~init:()
          ~f:(fun () () -> ())
      ;;

      let matching_string_gen (bp, lo, hi) =
        Blueprint.repeat_matching_string_gen ~sep:None ~bounds:(Some (lo, hi)) bp
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "list t ≡ fold t + List.rev" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t * Blueprint.Bound.t * Blueprint.Bound.t
      [@@deriving quickcheck, sexp_of]

      type result = Blueprint.Parse.t list [@@deriving compare, sexp_of]

      let lhs (bp, lo, hi) = Unstring.list ~bounds:(lo, hi) (Blueprint.to_unstring bp)

      let rhs (bp, lo, hi) =
        Unstring.fold ~bounds:(lo, hi) (Blueprint.to_unstring bp) ~init:[] ~f:(fun xs x ->
          x :: xs)
        |> Unstring.map ~f:List.rev
      ;;

      let matching_string_gen (bp, lo, hi) =
        Blueprint.repeat_matching_string_gen ~sep:None ~bounds:(Some (lo, hi)) bp
      ;;
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let%expect_test "lit s ≡ char-by-char via map2" =
  let module Test =
    Make (struct
      type testcase = string [@@deriving quickcheck, sexp_of]
      type result = unit [@@deriving compare, sexp_of]

      let lhs s = Unstring.lit s

      let rhs s =
        List.fold (String.to_list s) ~init:(Unstring.return ()) ~f:(fun acc c ->
          Unstring.map2 acc (Unstring.char (Char c)) ~f:(fun () () -> ()))
      ;;

      let matching_string_gen s = Some (Quickcheck.Generator.return s)
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;

let rec shape_to_unit_unstring (t : Unstring.Printable_shape.t) : unit Unstring.t =
  match t with
  | Literal s -> Unstring.lit s
  | Terminal cf -> Unstring.char cf
  | Capture t -> shape_to_unit_unstring t
  | Union ts -> Unstring.choice (List.map ts ~f:shape_to_unit_unstring)
  | Concat ts -> Unstring.all_unit (List.map ts ~f:shape_to_unit_unstring)
  | Star t -> Unstring.star (shape_to_unit_unstring t)
;;

let%expect_test "shape round-trip: t ≡ shape(t) reconstructed" =
  let module Test =
    Make (struct
      type testcase = Blueprint.t [@@deriving quickcheck, sexp_of]
      type result = unit [@@deriving compare, sexp_of]

      let lhs bp = Blueprint.to_unit_unstring bp

      let rhs bp =
        Blueprint.to_unit_unstring bp
        |> Unstring.to_printable_shape
        |> shape_to_unit_unstring
      ;;

      let matching_string_gen bp = Blueprint.matching_string_gen bp
    end)
  in
  let%quick_test prop (test : Test.t) = Test.validate test in
  [%expect {| |}]
;;
