(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core
open Helpers

let dot = File_path.Part.dot
let dot_dot = File_path.Part.dot_dot

let%expect_test _ =
  test_constants (module File_path.Part) [ dot; dot_dot ];
  [%expect
    {|
    .
    ..
    |}]
;;

open struct
  module Common = Test_common.Make (struct
      module Type = File_path.Types.Part
      module Path = File_path.Part
      module Examples = Examples.Part
      module Tested = Test_part_completion
    end)
end

include Common.Tested

let%expect_test _ =
  Common.run_expect_tests ();
  [%expect
    {|
    Testing: compare
    ("\001\255"
     -dot-is-not-always-first
     .
     ..
     .hidden
     "This is a sentence; it has punctuation, capitalization, and spaces!"
     bin
     bin.exe
     binary
     filename.txt
     "\255\001")

    Testing: of_string
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (! ("File_path.Part.of_string: invalid string" ""))
    (! ("File_path.Part.of_string: invalid string" invalid/slash))
    (! ("File_path.Part.of_string: invalid string" "invalid\000null"))

    Testing: containers
    (Set
     ("\001\255"
      .
      ..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      filename.txt))
    (Map
     (("\001\255" 0)
      (. 1)
      (.. 2)
      (.hidden 3)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 4)
      (bin 5)
      (filename.txt 6)))
    (Hash_set
     ("\001\255"
      .
      ..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      filename.txt))
    (Table
     (("\001\255" 0)
      (. 1)
      (.. 2)
      (.hidden 3)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 4)
      (bin 5)
      (filename.txt 6)))

    Testing: invariant
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (! ("File_path.Part.invariant: invalid string" ""))
    (! ("File_path.Part.invariant: invalid string" invalid/slash))
    (! ("File_path.Part.invariant: invalid string" "invalid\000null"))
    |}]
;;

[%%template
let append_to_basename_exn = (File_path.Part.append_to_basename_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    append_to_basename_exn
    (append_to_basename_exn [@alloc stack])
    (module Fn2_local_exn (File_path.Part) (String) (File_path.Part))
    ~examples:Examples.Part.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok append_to_basename_exn)
        (Option.try_with (fun () -> of_string (to_string path ^ string))));
  [%expect
    {|
    ((. x) -> (Ok .x))
    ((.. y) -> (Ok ..y))
    ((a .b) -> (Ok a.b))
    ((b invalid/slash)
     ->
     (Error
      ("File_path.Part.append_to_basename_exn: suffix contains invalid characters"
       ((path b) (suffix invalid/slash)))))
    ((c "invalid\000null")
     ->
     (Error
      ("File_path.Part.append_to_basename_exn: suffix contains invalid characters"
       ((path c) (suffix "invalid\000null")))))
    ((long-hyphenated-name-ending-in -this)
     ->
     (Ok long-hyphenated-name-ending-in-this))
    |}]
;;]
