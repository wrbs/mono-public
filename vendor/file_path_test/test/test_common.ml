open! Core
open Helpers
include Test_common_intf.Definitions

module Make (Arg : Arg) : S with module Tested.Type := Arg.Type = struct
  let expect_tests = Queue.create ()
  let enqueue_expect_test name runme = Queue.enqueue expect_tests (name, runme)

  module Tested = struct
    type t = Arg.Path.t
    [@@deriving
      compare ~localize
      , equal ~localize
      , globalize
      , sexp_of ~stackify
      , quickcheck
      , sexp_grammar]

    include Arg.Tested

    include%template (
      Arg.Path :
        Identifiable.S
        [@alloc stack] [@mode local portable]
        with type t := Arg.Type.t
         and type comparator_witness = Arg.Type.comparator_witness)

    let%template[@alloc stack] of_string = (Arg.Path.of_string [@alloc stack])

    let () =
      enqueue_expect_test "compare" (fun () ->
        test_compare (module Arg.Path) Arg.Examples.for_compare)
    ;;

    let () =
      enqueue_expect_test "of_string" (fun () ->
        test_of_string (module Arg.Path) Arg.Examples.strings_for_of_string)
    ;;

    let () =
      enqueue_expect_test "containers" (fun () ->
        test_containers (module Arg.Path) Arg.Examples.for_conversion)
    ;;

    module Expert = struct
      let%template unchecked_of_canonical_string =
        (Arg.Path.Expert.unchecked_of_canonical_string [@alloc a])
      [@@alloc a = (stack, heap)]
      ;;
    end

    let invariant = Arg.Path.invariant

    let () =
      enqueue_expect_test "invariant" (fun () ->
        test_invariant (module Arg.Path) Arg.Examples.strings_for_of_string)
    ;;
  end

  let run_expect_tests () =
    Queue.iter expect_tests ~f:(fun (name, runme) ->
      printf "\nTesting: %s\n" name;
      runme ())
  ;;
end
