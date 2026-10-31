(* We constrain these modules to provide the full interface of [File_path] and its
   submodules to make sure we test (nearly) every function. We do not test all ppx-derived
   or functor-generated functions, but otherwise we test everything. Any time a new
   binding is added to the library, it is required here; every binding added here should
   come with a test unless there is a compelling reason not to.

   We use [Helpers] for all tests. In tests requiring a [correctness] callback, we provide
   one testing the function against previously tested functions in the library. We only
   leave [correctness] tests empty if all functions it is tested against come below.

   We provide [~message] or [~if_false_then_print_s] arguments to all [require*]
   functions, mostly to make the purpose of tests more readable for reviewers. *)

open! Core
open Expect_test_helpers_core
open Helpers

let root = File_path.root
let dot = File_path.dot
let dot_dot = File_path.dot_dot

let%expect_test _ =
  test_constants (module File_path) [ root; dot; dot_dot ];
  [%expect
    {|
    /
    .
    ..
    |}]
;;

open struct
  module Common = Test_common.Make (struct
      module Type = File_path.Types.Path
      module Path = File_path
      module Examples = Examples.Path
      module Tested = Test_path_completion
    end)
end

include Common.Tested

let%expect_test _ =
  Common.run_expect_tests ();
  [%expect
    {|
    Testing: compare
    (/
     "/\001\255"
     /-dot-is-not-always-first
     /.
     /./.
     /././.
     /./..
     /..
     /../.
     /../..
     /.hidden
     "/This is a sentence; it has punctuation, capitalization, and spaces!"
     /bin
     /bin/exe
     /bin/exe/file
     /bin/exe.file
     /bin.exe
     /binary
     /filename.txt
     "/\255\001"
     "\001\255"
     -dot-is-not-always-first
     .
     ./.
     ././.
     ./..
     ..
     ../.
     ../..
     .hidden
     "This is a sentence; it has punctuation, capitalization, and spaces!"
     bin
     bin/exe
     bin/exe/file
     bin/exe.file
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
    (= ./.)
    (= ../..)
    (= ././.)
    (= bin/exe)
    (= bin/exe/file)
    (= /)
    (= /.)
    (= /..)
    (= /filename.txt)
    (= /bin)
    (= /.hidden)
    (= "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "/\001\255")
    (= /./.)
    (= /../..)
    (= /././.)
    (= /bin/exe)
    (= /bin/exe/file)
    (~ ./ .)
    (~ .//. ./.)
    (~ .//.// ./.)
    (~ bin/exe/ bin/exe)
    (~ bin//exe//file bin/exe/file)
    (~ bin//exe//file/ bin/exe/file)
    (~ // /)
    (~ //. /.)
    (~ /./ /.)
    (~ /.//. /./.)
    (~ /.//.// /./.)
    (~ /bin/exe/ /bin/exe)
    (~ /bin//exe//file /bin/exe/file)
    (~ /bin//exe//file/ /bin/exe/file)
    (! ("File_path.of_string: invalid string" ""))
    (! ("File_path.of_string: invalid string" "invalid/\000/null"))

    Testing: containers
    (Set
     (/
      "/\001\255"
      /.
      /./.
      /././.
      /..
      /../..
      /.hidden
      "/This is a sentence; it has punctuation, capitalization, and spaces!"
      /bin
      /bin/exe
      /bin/exe/file
      /filename.txt
      "\001\255"
      .
      ./.
      ././.
      ..
      ../..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      bin/exe
      bin/exe/file
      filename.txt))
    (Map
     ((/ 0)
      ("/\001\255" 1)
      (/. 2)
      (/./. 3)
      (/././. 4)
      (/.. 5)
      (/../.. 6)
      (/.hidden 7)
      ("/This is a sentence; it has punctuation, capitalization, and spaces!" 8)
      (/bin 9)
      (/bin/exe 10)
      (/bin/exe/file 11)
      (/filename.txt 12)
      ("\001\255" 13)
      (. 14)
      (./. 15)
      (././. 16)
      (.. 17)
      (../.. 18)
      (.hidden 19)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 20)
      (bin 21)
      (bin/exe 22)
      (bin/exe/file 23)
      (filename.txt 24)))
    (Hash_set
     (/
      "/\001\255"
      /.
      /./.
      /././.
      /..
      /../..
      /.hidden
      "/This is a sentence; it has punctuation, capitalization, and spaces!"
      /bin
      /bin/exe
      /bin/exe/file
      /filename.txt
      "\001\255"
      .
      ./.
      ././.
      ..
      ../..
      .hidden
      "This is a sentence; it has punctuation, capitalization, and spaces!"
      bin
      bin/exe
      bin/exe/file
      filename.txt))
    (Table
     ((/ 0)
      ("/\001\255" 1)
      (/. 2)
      (/./. 3)
      (/././. 4)
      (/.. 5)
      (/../.. 6)
      (/.hidden 7)
      ("/This is a sentence; it has punctuation, capitalization, and spaces!" 8)
      (/bin 9)
      (/bin/exe 10)
      (/bin/exe/file 11)
      (/filename.txt 12)
      ("\001\255" 13)
      (. 14)
      (./. 15)
      (././. 16)
      (.. 17)
      (../.. 18)
      (.hidden 19)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 20)
      (bin 21)
      (bin/exe 22)
      (bin/exe/file 23)
      (filename.txt 24)))

    Testing: invariant
    (= .)
    (= ..)
    (= filename.txt)
    (= bin)
    (= .hidden)
    (= "This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "\001\255")
    (= ./.)
    (= ../..)
    (= ././.)
    (= bin/exe)
    (= bin/exe/file)
    (= /)
    (= /.)
    (= /..)
    (= /filename.txt)
    (= /bin)
    (= /.hidden)
    (= "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (= "/\001\255")
    (= /./.)
    (= /../..)
    (= /././.)
    (= /bin/exe)
    (= /bin/exe/file)
    (! ("File_path.invariant: non-canonical representation" ./))
    (! ("File_path.invariant: non-canonical representation" .//.))
    (! ("File_path.invariant: non-canonical representation" .//.//))
    (! ("File_path.invariant: non-canonical representation" bin/exe/))
    (! ("File_path.invariant: non-canonical representation" bin//exe//file))
    (! ("File_path.invariant: non-canonical representation" bin//exe//file/))
    (! ("File_path.invariant: non-canonical representation" //))
    (! ("File_path.invariant: non-canonical representation" //.))
    (! ("File_path.invariant: non-canonical representation" /./))
    (! ("File_path.invariant: non-canonical representation" /.//.))
    (! ("File_path.invariant: non-canonical representation" /.//.//))
    (! ("File_path.invariant: non-canonical representation" /bin/exe/))
    (! ("File_path.invariant: non-canonical representation" /bin//exe//file))
    (! ("File_path.invariant: non-canonical representation" /bin//exe//file/))
    (! ("File_path.invariant: invalid string" ""))
    (! ("File_path.invariant: invalid string" "invalid/\000/null"))
    |}]
;;

[%%template
let is_relative = File_path.is_relative

let%expect_test _ =
  test_predicate
    is_relative
    (module Fn_local (File_path) (Bool))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (.
       ..
       filename.txt
       bin
       .hidden
       "This is a sentence; it has punctuation, capitalization, and spaces!"
       "\001\255"
       ./.
       ../..
       ././.
       bin/exe
       bin/exe/file))
     (failure
      (/
       /.
       /..
       /filename.txt
       /bin
       /.hidden
       "/This is a sentence; it has punctuation, capitalization, and spaces!"
       "/\001\255"
       /./.
       /../..
       /././.
       /bin/exe
       /bin/exe/file)))
    |}]
;;

let is_absolute = File_path.is_absolute

let%expect_test _ =
  test_predicate
    is_absolute
    (module Fn_local (File_path) (Bool))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path is_absolute ->
      require_equal
        (module Bool)
        is_absolute
        (not (is_relative path))
        ~message:"[is_absolute] and [is_relative] are inconsistent");
  [%expect
    {|
    ((success
      (/
       /.
       /..
       /filename.txt
       /bin
       /.hidden
       "/This is a sentence; it has punctuation, capitalization, and spaces!"
       "/\001\255"
       /./.
       /../..
       /././.
       /bin/exe
       /bin/exe/file))
     (failure
      (.
       ..
       filename.txt
       bin
       .hidden
       "This is a sentence; it has punctuation, capitalization, and spaces!"
       "\001\255"
       ./.
       ../..
       ././.
       bin/exe
       bin/exe/file)))
    |}]
;;

let to_absolute = (File_path.to_absolute [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_absolute
    (to_absolute [@alloc stack])
    (module Fn (File_path) (Option_of (File_path.Absolute)))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_absolute ->
      require_equal
        (module Bool)
        (Option.is_some to_absolute)
        (is_absolute path)
        ~message:"[to_absolute] and [is_absolute] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (filename.txt -> ())
    (bin -> ())
    (.hidden -> ())
    ("This is a sentence; it has punctuation, capitalization, and spaces!" -> ())
    ("\001\255" -> ())
    (./. -> ())
    (../.. -> ())
    (././. -> ())
    (bin/exe -> ())
    (bin/exe/file -> ())
    (/ -> (/))
    (/. -> (/.))
    (/.. -> (/..))
    (/filename.txt -> (/filename.txt))
    (/bin -> (/bin))
    (/.hidden -> (/.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> ("/\001\255"))
    (/./. -> (/./.))
    (/../.. -> (/../..))
    (/././. -> (/././.))
    (/bin/exe -> (/bin/exe))
    (/bin/exe/file -> (/bin/exe/file))
    |}]
;;

let to_relative = (File_path.to_relative [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_relative
    (to_relative [@alloc stack])
    (module Fn (File_path) (Option_of (File_path.Relative)))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_relative ->
      require_equal
        (module Bool)
        (Option.is_some to_relative)
        (is_relative path)
        ~message:"[to_relative] and [is_relative] are inconsistent");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (filename.txt -> (filename.txt))
    (bin -> (bin))
    (.hidden -> (.hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> ("\001\255"))
    (./. -> (./.))
    (../.. -> (../..))
    (././. -> (././.))
    (bin/exe -> (bin/exe))
    (bin/exe/file -> (bin/exe/file))
    (/ -> ())
    (/. -> ())
    (/.. -> ())
    (/filename.txt -> ())
    (/bin -> ())
    (/.hidden -> ())
    ("/This is a sentence; it has punctuation, capitalization, and spaces!" -> ())
    ("/\001\255" -> ())
    (/./. -> ())
    (/../.. -> ())
    (/././. -> ())
    (/bin/exe -> ())
    (/bin/exe/file -> ())
    |}]
;;

let to_absolute_exn = (File_path.to_absolute_exn [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    to_absolute_exn
    (to_absolute_exn [@mode local])
    (module Fn_exn (File_path) (File_path.Absolute))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_absolute_exn ->
      require_equal
        (module Option_of (File_path.Absolute))
        (Or_error.ok to_absolute_exn)
        (to_absolute path)
        ~message:"[to_absolute_exn] and [to_absolute] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.to_absolute_exn: path is relative" .)))
    (.. -> (Error ("File_path.to_absolute_exn: path is relative" ..)))
    (filename.txt
     ->
     (Error ("File_path.to_absolute_exn: path is relative" filename.txt)))
    (bin -> (Error ("File_path.to_absolute_exn: path is relative" bin)))
    (.hidden -> (Error ("File_path.to_absolute_exn: path is relative" .hidden)))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_absolute_exn: path is relative"
       "This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("\001\255"
     ->
     (Error ("File_path.to_absolute_exn: path is relative" "\001\255")))
    (./. -> (Error ("File_path.to_absolute_exn: path is relative" ./.)))
    (../.. -> (Error ("File_path.to_absolute_exn: path is relative" ../..)))
    (././. -> (Error ("File_path.to_absolute_exn: path is relative" ././.)))
    (bin/exe -> (Error ("File_path.to_absolute_exn: path is relative" bin/exe)))
    (bin/exe/file
     ->
     (Error ("File_path.to_absolute_exn: path is relative" bin/exe/file)))
    (/ -> (Ok /))
    (/. -> (Ok /.))
    (/.. -> (Ok /..))
    (/filename.txt -> (Ok /filename.txt))
    (/bin -> (Ok /bin))
    (/.hidden -> (Ok /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Ok "/\001\255"))
    (/./. -> (Ok /./.))
    (/../.. -> (Ok /../..))
    (/././. -> (Ok /././.))
    (/bin/exe -> (Ok /bin/exe))
    (/bin/exe/file -> (Ok /bin/exe/file))
    |}]
;;

let to_relative_exn = (File_path.to_relative_exn [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    to_relative_exn
    (to_relative_exn [@mode local])
    (module Fn_exn (File_path) (File_path.Relative))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_relative_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok to_relative_exn)
        (to_relative path)
        ~message:"[to_relative_exn] and [to_relative] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (filename.txt -> (Ok filename.txt))
    (bin -> (Ok bin))
    (.hidden -> (Ok .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Ok "\001\255"))
    (./. -> (Ok ./.))
    (../.. -> (Ok ../..))
    (././. -> (Ok ././.))
    (bin/exe -> (Ok bin/exe))
    (bin/exe/file -> (Ok bin/exe/file))
    (/ -> (Error ("File_path.to_relative_exn: path is absolute" /)))
    (/. -> (Error ("File_path.to_relative_exn: path is absolute" /.)))
    (/.. -> (Error ("File_path.to_relative_exn: path is absolute" /..)))
    (/filename.txt
     ->
     (Error ("File_path.to_relative_exn: path is absolute" /filename.txt)))
    (/bin -> (Error ("File_path.to_relative_exn: path is absolute" /bin)))
    (/.hidden -> (Error ("File_path.to_relative_exn: path is absolute" /.hidden)))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_relative_exn: path is absolute"
       "/This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("/\001\255"
     ->
     (Error ("File_path.to_relative_exn: path is absolute" "/\001\255")))
    (/./. -> (Error ("File_path.to_relative_exn: path is absolute" /./.)))
    (/../.. -> (Error ("File_path.to_relative_exn: path is absolute" /../..)))
    (/././. -> (Error ("File_path.to_relative_exn: path is absolute" /././.)))
    (/bin/exe -> (Error ("File_path.to_relative_exn: path is absolute" /bin/exe)))
    (/bin/exe/file
     ->
     (Error ("File_path.to_relative_exn: path is absolute" /bin/exe/file)))
    |}]
;;

let to_absolute_or_error = (File_path.to_absolute_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    to_absolute_or_error
    (to_absolute_or_error [@alloc stack])
    (module Fn_or_error (File_path) (File_path.Absolute))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_absolute_or_error ->
      require_equal
        (module Option_of (File_path.Absolute))
        (Or_error.ok to_absolute_or_error)
        (to_absolute path)
        ~message:"[to_absolute_or_error] and [to_absolute] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.to_absolute_or_error: path is relative" .)))
    (.. -> (Error ("File_path.to_absolute_or_error: path is relative" ..)))
    (filename.txt
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" filename.txt)))
    (bin -> (Error ("File_path.to_absolute_or_error: path is relative" bin)))
    (.hidden
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" .hidden)))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_absolute_or_error: path is relative"
       "This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("\001\255"
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" "\001\255")))
    (./. -> (Error ("File_path.to_absolute_or_error: path is relative" ./.)))
    (../.. -> (Error ("File_path.to_absolute_or_error: path is relative" ../..)))
    (././. -> (Error ("File_path.to_absolute_or_error: path is relative" ././.)))
    (bin/exe
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" bin/exe)))
    (bin/exe/file
     ->
     (Error ("File_path.to_absolute_or_error: path is relative" bin/exe/file)))
    (/ -> (Ok /))
    (/. -> (Ok /.))
    (/.. -> (Ok /..))
    (/filename.txt -> (Ok /filename.txt))
    (/bin -> (Ok /bin))
    (/.hidden -> (Ok /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Ok "/\001\255"))
    (/./. -> (Ok /./.))
    (/../.. -> (Ok /../..))
    (/././. -> (Ok /././.))
    (/bin/exe -> (Ok /bin/exe))
    (/bin/exe/file -> (Ok /bin/exe/file))
    |}]
;;

let to_relative_or_error = (File_path.to_relative_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    to_relative_or_error
    (to_relative_or_error [@alloc stack])
    (module Fn_or_error (File_path) (File_path.Relative))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_relative_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok to_relative_or_error)
        (to_relative path)
        ~message:"[to_relative_or_error] and [to_relative] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (filename.txt -> (Ok filename.txt))
    (bin -> (Ok bin))
    (.hidden -> (Ok .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Ok "\001\255"))
    (./. -> (Ok ./.))
    (../.. -> (Ok ../..))
    (././. -> (Ok ././.))
    (bin/exe -> (Ok bin/exe))
    (bin/exe/file -> (Ok bin/exe/file))
    (/ -> (Error ("File_path.to_relative_or_error: path is absolute" /)))
    (/. -> (Error ("File_path.to_relative_or_error: path is absolute" /.)))
    (/.. -> (Error ("File_path.to_relative_or_error: path is absolute" /..)))
    (/filename.txt
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /filename.txt)))
    (/bin -> (Error ("File_path.to_relative_or_error: path is absolute" /bin)))
    (/.hidden
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /.hidden)))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Error
      ("File_path.to_relative_or_error: path is absolute"
       "/This is a sentence; it has punctuation, capitalization, and spaces!")))
    ("/\001\255"
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" "/\001\255")))
    (/./. -> (Error ("File_path.to_relative_or_error: path is absolute" /./.)))
    (/../..
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /../..)))
    (/././.
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /././.)))
    (/bin/exe
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /bin/exe)))
    (/bin/exe/file
     ->
     (Error ("File_path.to_relative_or_error: path is absolute" /bin/exe/file)))
    |}]
;;

let of_absolute = (File_path.of_absolute [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    of_absolute
    (of_absolute [@mode local])
    (module Fn (File_path.Absolute) (File_path))
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun absolute path ->
      require_equal
        (module Option_of (File_path.Absolute))
        (to_absolute path)
        (Some absolute)
        ~message:"[of_absolute] and [to_absolute] are inconsistent");
  [%expect
    {|
    (/ -> /)
    (/. -> /.)
    (/.. -> /..)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/./. -> /./.)
    (/../.. -> /../..)
    (/././. -> /././.)
    (/bin/exe -> /bin/exe)
    (/bin/exe/file -> /bin/exe/file)
    |}]
;;

let of_relative = (File_path.of_relative [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    of_relative
    (of_relative [@mode local])
    (module Fn (File_path.Relative) (File_path))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative path ->
      require_equal
        (module Option_of (File_path.Relative))
        (to_relative path)
        (Some relative)
        ~message:"[of_relative] and [to_relative] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    (./. -> ./.)
    (../.. -> ../..)
    (././. -> ././.)
    (bin/exe -> bin/exe)
    (bin/exe/file -> bin/exe/file)
    |}]
;;

let of_part_relative = (File_path.of_part_relative [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    of_part_relative
    (of_part_relative [@mode local])
    (module Fn (File_path.Part) (File_path))
    ~examples:Examples.Part.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    |}]
;;

let number_of_parts = File_path.number_of_parts

let%expect_test _ =
  test_immediate
    number_of_parts
    (module Fn_local (File_path) (Int))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    (. -> 1)
    (.. -> 1)
    (filename.txt -> 1)
    (bin -> 1)
    (.hidden -> 1)
    ("This is a sentence; it has punctuation, capitalization, and spaces!" -> 1)
    ("\001\255" -> 1)
    (./. -> 2)
    (../.. -> 2)
    (././. -> 3)
    (bin/exe -> 2)
    (bin/exe/file -> 3)
    (/ -> 0)
    (/. -> 1)
    (/.. -> 1)
    (/filename.txt -> 1)
    (/bin -> 1)
    (/.hidden -> 1)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!" -> 1)
    ("/\001\255" -> 1)
    (/./. -> 2)
    (/../.. -> 2)
    (/././. -> 3)
    (/bin/exe -> 2)
    (/bin/exe/file -> 3)
    |}]
;;

let basename = (File_path.basename [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    basename
    (basename [@alloc stack])
    (module Fn (File_path) (Option_of (File_path.Part)))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path basename ->
      require_equal
        (module Bool)
        (Option.is_none basename)
        (equal path root)
        ~message:"[basename] is inconsistent with [equal root]");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (singleton -> (singleton))
    (./file -> (file))
    (dir/. -> (.))
    (../.. -> (..))
    (a/b -> (b))
    (a/b/c -> (c))
    (a/b/c/d -> (d))
    (long/chain/of/names/ending/in/this -> (this))
    (/ -> ())
    (/. -> (.))
    (/.. -> (..))
    (/singleton -> (singleton))
    (/./file -> (file))
    (/dir/. -> (.))
    (/../.. -> (..))
    (/a/b -> (b))
    (/a/b/c -> (c))
    (/a/b/c/d -> (d))
    (/long/chain/of/names/ending/in/this -> (this))
    |}]
;;

let basename_exn = (File_path.basename_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    basename_exn
    (basename_exn [@alloc stack])
    (module Fn_exn (File_path) (File_path.Part))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path basename_exn ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok basename_exn)
        (basename path)
        ~message:"[basename_exn] and [basename] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (singleton -> (Ok singleton))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok c))
    (a/b/c/d -> (Ok d))
    (long/chain/of/names/ending/in/this -> (Ok this))
    (/ -> (Error "File_path.basename_exn: root path"))
    (/. -> (Ok .))
    (/.. -> (Ok ..))
    (/singleton -> (Ok singleton))
    (/./file -> (Ok file))
    (/dir/. -> (Ok .))
    (/../.. -> (Ok ..))
    (/a/b -> (Ok b))
    (/a/b/c -> (Ok c))
    (/a/b/c/d -> (Ok d))
    (/long/chain/of/names/ending/in/this -> (Ok this))
    |}]
;;

let basename_or_error = (File_path.basename_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    basename_or_error
    (basename_or_error [@alloc stack])
    (module Fn_or_error (File_path) (File_path.Part))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path basename_or_error ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok basename_or_error)
        (basename path)
        ~message:"[basename_or_error] and [basename] are inconsistent");
  [%expect
    {|
    (. -> (Ok .))
    (.. -> (Ok ..))
    (singleton -> (Ok singleton))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok c))
    (a/b/c/d -> (Ok d))
    (long/chain/of/names/ending/in/this -> (Ok this))
    (/ -> (Error "File_path.basename_or_error: root path"))
    (/. -> (Ok .))
    (/.. -> (Ok ..))
    (/singleton -> (Ok singleton))
    (/./file -> (Ok file))
    (/dir/. -> (Ok .))
    (/../.. -> (Ok ..))
    (/a/b -> (Ok b))
    (/a/b/c -> (Ok c))
    (/a/b/c/d -> (Ok d))
    (/long/chain/of/names/ending/in/this -> (Ok this))
    |}]
;;

let basename_defaulting_to_dot = (File_path.basename_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    basename_defaulting_to_dot
    (basename_defaulting_to_dot [@alloc stack])
    (module Fn (File_path) (File_path.Part))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path basename_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        basename_defaulting_to_dot
        (Option.value (basename path) ~default:File_path.Part.dot)
        ~message:"[basename_defaulting_to_dot] and [basename] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (singleton -> singleton)
    (./file -> file)
    (dir/. -> .)
    (../.. -> ..)
    (a/b -> b)
    (a/b/c -> c)
    (a/b/c/d -> d)
    (long/chain/of/names/ending/in/this -> this)
    (/ -> .)
    (/. -> .)
    (/.. -> ..)
    (/singleton -> singleton)
    (/./file -> file)
    (/dir/. -> .)
    (/../.. -> ..)
    (/a/b -> b)
    (/a/b/c -> c)
    (/a/b/c/d -> d)
    (/long/chain/of/names/ending/in/this -> this)
    |}]
;;

let dirname = (File_path.dirname [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname
    (dirname [@alloc stack])
    (module Fn (File_path) (Option_of (File_path)))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Int.equal (number_of_parts path) (if is_absolute path then 0 else 1))
        ~message:"[dirname] is not [Some] in the right cases");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (.))
    (dir/. -> (dir))
    (../.. -> (..))
    (a/b -> (a))
    (a/b/c -> (a/b))
    (a/b/c/d -> (a/b/c))
    (long/chain/of/names/ending/in/this -> (long/chain/of/names/ending/in))
    (/ -> ())
    (/. -> (/))
    (/.. -> (/))
    (/singleton -> (/))
    (/./file -> (/.))
    (/dir/. -> (/dir))
    (/../.. -> (/..))
    (/a/b -> (/a))
    (/a/b/c -> (/a/b))
    (/a/b/c/d -> (/a/b/c))
    (/long/chain/of/names/ending/in/this -> (/long/chain/of/names/ending/in))
    |}]
;;

let dirname_exn = (File_path.dirname_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname_exn
    (dirname_exn [@alloc stack])
    (module Fn_exn (File_path) (File_path))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path dirname_exn ->
      require_equal
        (module Option_of (File_path))
        (Or_error.ok dirname_exn)
        (dirname path)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.dirname_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.dirname_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.dirname_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    (/ -> (Error "File_path.dirname_exn: root path"))
    (/. -> (Ok /))
    (/.. -> (Ok /))
    (/singleton -> (Ok /))
    (/./file -> (Ok /.))
    (/dir/. -> (Ok /dir))
    (/../.. -> (Ok /..))
    (/a/b -> (Ok /a))
    (/a/b/c -> (Ok /a/b))
    (/a/b/c/d -> (Ok /a/b/c))
    (/long/chain/of/names/ending/in/this -> (Ok /long/chain/of/names/ending/in))
    |}]
;;

let dirname_or_error = (File_path.dirname_or_error [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname_or_error
    (dirname_or_error [@alloc stack])
    (module Fn_or_error (File_path) (File_path))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path dirname_or_error ->
      require_equal
        (module Option_of (File_path))
        (Or_error.ok dirname_or_error)
        (dirname path)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.dirname_or_error: path contains no slash" .)))
    (.. -> (Error ("File_path.dirname_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.dirname_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    (/ -> (Error "File_path.dirname_or_error: root path"))
    (/. -> (Ok /))
    (/.. -> (Ok /))
    (/singleton -> (Ok /))
    (/./file -> (Ok /.))
    (/dir/. -> (Ok /dir))
    (/../.. -> (Ok /..))
    (/a/b -> (Ok /a))
    (/a/b/c -> (Ok /a/b))
    (/a/b/c/d -> (Ok /a/b/c))
    (/long/chain/of/names/ending/in/this -> (Ok /long/chain/of/names/ending/in))
    |}]
;;

let dirname_defaulting_to_dot_or_root =
  (File_path.dirname_defaulting_to_dot_or_root [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_defaulting_to_dot_or_root
    (dirname_defaulting_to_dot_or_root [@alloc stack])
    (module Fn (File_path) (File_path))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path dirname_defaulting_to_dot_or_root ->
      require_equal
        (module File_path)
        dirname_defaulting_to_dot_or_root
        (Option.value (dirname path) ~default:(if is_relative path then dot else root))
        ~message:"[dirname_defaulting_to_dot_or_root] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> .)
    (singleton -> .)
    (./file -> .)
    (dir/. -> dir)
    (../.. -> ..)
    (a/b -> a)
    (a/b/c -> a/b)
    (a/b/c/d -> a/b/c)
    (long/chain/of/names/ending/in/this -> long/chain/of/names/ending/in)
    (/ -> /)
    (/. -> /)
    (/.. -> /)
    (/singleton -> /)
    (/./file -> /.)
    (/dir/. -> /dir)
    (/../.. -> /..)
    (/a/b -> /a)
    (/a/b/c -> /a/b)
    (/a/b/c/d -> /a/b/c)
    (/long/chain/of/names/ending/in/this -> /long/chain/of/names/ending/in)
    |}]
;;

let dirname_and_basename = (File_path.dirname_and_basename [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_and_basename
    (dirname_and_basename [@alloc stack])
    (module Fn (File_path) (Option_of (Pair_of (File_path) (File_path.Part))))
    ~examples:Examples.Path.for_basename_and_dirname
    ~correctness:(fun path dirname_and_basename ->
      require_equal
        (module Option_of (Pair_of (File_path) (File_path.Part)))
        dirname_and_basename
        (Option.both (dirname path) (basename path))
        ~message:"[dirname_and_basename] and [dirname]/[basename] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> ((. file)))
    (dir/. -> ((dir .)))
    (../.. -> ((.. ..)))
    (a/b -> ((a b)))
    (a/b/c -> ((a/b c)))
    (a/b/c/d -> ((a/b/c d)))
    (long/chain/of/names/ending/in/this -> ((long/chain/of/names/ending/in this)))
    (/ -> ())
    (/. -> ((/ .)))
    (/.. -> ((/ ..)))
    (/singleton -> ((/ singleton)))
    (/./file -> ((/. file)))
    (/dir/. -> ((/dir .)))
    (/../.. -> ((/.. ..)))
    (/a/b -> ((/a b)))
    (/a/b/c -> ((/a/b c)))
    (/a/b/c/d -> ((/a/b/c d)))
    (/long/chain/of/names/ending/in/this
     ->
     ((/long/chain/of/names/ending/in this)))
    |}]
;;

let append_to_basename_exn = (File_path.append_to_basename_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    append_to_basename_exn
    (append_to_basename_exn [@alloc stack])
    (module Fn2_local_exn (File_path) (String) (File_path))
    ~examples:Examples.Path.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Option_of (File_path))
        (Or_error.ok append_to_basename_exn)
        (if equal path root || String.mem string '/' || String.mem string '\000'
         then None
         else Some (of_string (to_string path ^ string))));
  [%expect
    {|
    ((. x) -> (Ok .x))
    ((.. y) -> (Ok ..y))
    ((a .b) -> (Ok a.b))
    ((a/b .c) -> (Ok a/b.c))
    ((a/b/c .d) -> (Ok a/b/c.d))
    ((a/b/c "") -> (Ok a/b/c))
    ((a/b/c invalid/slash)
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix invalid/slash)))))
    ((a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix "invalid\000null")))))
    ((long/chain/of/names/ending/in -this)
     ->
     (Ok long/chain/of/names/ending/in-this))
    ((/ "")
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "")))))
    ((/ x)
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix x)))))
    ((/ invalid/slash)
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix invalid/slash)))))
    ((/ "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "invalid\000null")))))
    ((/. x) -> (Ok /.x))
    ((/.. y) -> (Ok /..y))
    ((/a .b) -> (Ok /a.b))
    ((/a/b .c) -> (Ok /a/b.c))
    ((/a/b/c .d) -> (Ok /a/b/c.d))
    ((/a/b/c "") -> (Ok /a/b/c))
    ((/a/b/c invalid/slash)
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix invalid/slash)))))
    ((/a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix "invalid\000null")))))
    ((/long/chain/of/names/ending/in -this)
     ->
     (Ok /long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = (File_path.append_part [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append_part
    (append_part [@alloc stack])
    (module Fn2_local (File_path) (File_path.Part) (File_path))
    ~examples:Examples.Path.for_append_part
    ~correctness:(fun (path, part) append_part ->
      require_equal
        (module Option_of (File_path))
        (dirname append_part)
        (Some path)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module Option_of (File_path.Part))
        (basename append_part)
        (Some part)
        ~message:"[append_part] and [basename] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b) -> a/b)
    ((a/b c) -> a/b/c)
    ((a/b/c d) -> a/b/c/d)
    ((long/chain/of/names/ending/in this) -> long/chain/of/names/ending/in/this)
    ((/ .) -> /.)
    ((/ ..) -> /..)
    ((/ singleton) -> /singleton)
    ((/. file) -> /./file)
    ((/dir .) -> /dir/.)
    ((/.. ..) -> /../..)
    ((/a b) -> /a/b)
    ((/a/b c) -> /a/b/c)
    ((/a/b/c d) -> /a/b/c/d)
    ((/long/chain/of/names/ending/in this) -> /long/chain/of/names/ending/in/this)
    |}]
;;

let is_prefix = File_path.is_prefix

let%expect_test _ =
  test_predicate
    is_prefix
    (module Fn_labelled (With_prefix_local (File_path)) (Bool))
    ~examples:Examples.Path.for_chop_prefix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((path .) (prefix .))
       ((path ..) (prefix ..))
       ((path a/b/c) (prefix a/b/c))
       ((path ./file) (prefix .))
       ((path dir/.) (prefix dir))
       ((path ../..) (prefix ..))
       ((path a/b/c/d) (prefix a))
       ((path a/b/c/d) (prefix a/b))
       ((path a/b/c/d) (prefix a/b/c))
       ((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
       ((path /) (prefix /))
       ((path /.) (prefix /.))
       ((path /..) (prefix /..))
       ((path /a/b/c) (prefix /a/b/c))
       ((path /./file) (prefix /.))
       ((path /dir/.) (prefix /dir))
       ((path /../..) (prefix /..))
       ((path /a/b/c/d) (prefix /a))
       ((path /a/b/c/d) (prefix /a/b))
       ((path /a/b/c/d) (prefix /a/b/c))
       ((path /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))))
     (failure
      (((path ..) (prefix .))
       ((path ./b) (prefix ./a))
       ((path c/d) (prefix a/b))
       ((path a) (prefix a/b/c))
       ((path a/b) (prefix a/b/c))
       ((path /..) (prefix /.))
       ((path /./b) (prefix /./a))
       ((path /c/d) (prefix /a/b))
       ((path /a) (prefix /a/b/c))
       ((path /a/b) (prefix /a/b/c))
       ((path /) (prefix .))
       ((path .) (prefix /))
       ((path /a/b/c) (prefix a/b))
       ((path a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix = (File_path.chop_prefix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_prefix
    (chop_prefix [@alloc stack])
    (module Fn_labelled (With_prefix (File_path)) (Option_of (File_path.Relative)))
    ~examples:Examples.Path.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix ->
      require_equal
        (module Bool)
        (is_prefix path ~prefix)
        (Option.is_some chop_prefix)
        ~message:"[chop_prefix] and [is_prefix] are inconsistent");
  [%expect
    {|
    (((path ..) (prefix .)) -> ())
    (((path ./b) (prefix ./a)) -> ())
    (((path c/d) (prefix a/b)) -> ())
    (((path a) (prefix a/b/c)) -> ())
    (((path a/b) (prefix a/b/c)) -> ())
    (((path .) (prefix .)) -> (.))
    (((path ..) (prefix ..)) -> (.))
    (((path a/b/c) (prefix a/b/c)) -> (.))
    (((path ./file) (prefix .)) -> (file))
    (((path dir/.) (prefix dir)) -> (.))
    (((path ../..) (prefix ..)) -> (..))
    (((path a/b/c/d) (prefix a)) -> (b/c/d))
    (((path a/b/c/d) (prefix a/b)) -> (c/d))
    (((path a/b/c/d) (prefix a/b/c)) -> (d))
    (((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (ending/in/this))
    (((path /) (prefix /)) -> (.))
    (((path /..) (prefix /.)) -> ())
    (((path /./b) (prefix /./a)) -> ())
    (((path /c/d) (prefix /a/b)) -> ())
    (((path /a) (prefix /a/b/c)) -> ())
    (((path /a/b) (prefix /a/b/c)) -> ())
    (((path /.) (prefix /.)) -> (.))
    (((path /..) (prefix /..)) -> (.))
    (((path /a/b/c) (prefix /a/b/c)) -> (.))
    (((path /./file) (prefix /.)) -> (file))
    (((path /dir/.) (prefix /dir)) -> (.))
    (((path /../..) (prefix /..)) -> (..))
    (((path /a/b/c/d) (prefix /a)) -> (b/c/d))
    (((path /a/b/c/d) (prefix /a/b)) -> (c/d))
    (((path /a/b/c/d) (prefix /a/b/c)) -> (d))
    (((path /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (ending/in/this))
    (((path /) (prefix .)) -> ())
    (((path .) (prefix /)) -> ())
    (((path /a/b/c) (prefix a/b)) -> ())
    (((path a/b/c) (prefix /a/b)) -> ())
    |}]
;;

let chop_prefix_exn = (File_path.chop_prefix_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_prefix_exn
    (chop_prefix_exn [@alloc stack])
    (module Fn_labelled_exn (With_prefix (File_path)) (File_path.Relative))
    ~examples:Examples.Path.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_prefix path ~prefix)
        (Or_error.ok chop_prefix_exn)
        ~message:"[chop_prefix_exn] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path ..) (prefix .))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path ..) (prefix .)))))
    (((path ./b) (prefix ./a))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path ./b) (prefix ./a)))))
    (((path c/d) (prefix a/b))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path c/d) (prefix a/b)))))
    (((path a) (prefix a/b/c))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path a) (prefix a/b/c)))))
    (((path a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path a/b) (prefix a/b/c)))))
    (((path .) (prefix .)) -> (Ok .))
    (((path ..) (prefix ..)) -> (Ok .))
    (((path a/b/c) (prefix a/b/c)) -> (Ok .))
    (((path ./file) (prefix .)) -> (Ok file))
    (((path dir/.) (prefix dir)) -> (Ok .))
    (((path ../..) (prefix ..)) -> (Ok ..))
    (((path a/b/c/d) (prefix a)) -> (Ok b/c/d))
    (((path a/b/c/d) (prefix a/b)) -> (Ok c/d))
    (((path a/b/c/d) (prefix a/b/c)) -> (Ok d))
    (((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (prefix /)) -> (Ok .))
    (((path /..) (prefix /.))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path /..) (prefix /.)))))
    (((path /./b) (prefix /./a))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /./b) (prefix /./a)))))
    (((path /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /c/d) (prefix /a/b)))))
    (((path /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a) (prefix /a/b/c)))))
    (((path /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a/b) (prefix /a/b/c)))))
    (((path /.) (prefix /.)) -> (Ok .))
    (((path /..) (prefix /..)) -> (Ok .))
    (((path /a/b/c) (prefix /a/b/c)) -> (Ok .))
    (((path /./file) (prefix /.)) -> (Ok file))
    (((path /dir/.) (prefix /dir)) -> (Ok .))
    (((path /../..) (prefix /..)) -> (Ok ..))
    (((path /a/b/c/d) (prefix /a)) -> (Ok b/c/d))
    (((path /a/b/c/d) (prefix /a/b)) -> (Ok c/d))
    (((path /a/b/c/d) (prefix /a/b/c)) -> (Ok d))
    (((path /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (prefix .))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path /) (prefix .)))))
    (((path .) (prefix /))
     ->
     (Error ("File_path.chop_prefix_exn: not a prefix" ((path .) (prefix /)))))
    (((path /a/b/c) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path /a/b/c) (prefix a/b)))))
    (((path a/b/c) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_exn: not a prefix" ((path a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix_or_error = (File_path.chop_prefix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_or_error
    (chop_prefix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_prefix (File_path)) (File_path.Relative))
    ~examples:Examples.Path.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_prefix path ~prefix)
        (Or_error.ok chop_prefix_or_error)
        ~message:"[chop_prefix_or_error] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path ..) (prefix .))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path ..) (prefix .)))))
    (((path ./b) (prefix ./a))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path ./b) (prefix ./a)))))
    (((path c/d) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path c/d) (prefix a/b)))))
    (((path a) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path a) (prefix a/b/c)))))
    (((path a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path a/b) (prefix a/b/c)))))
    (((path .) (prefix .)) -> (Ok .))
    (((path ..) (prefix ..)) -> (Ok .))
    (((path a/b/c) (prefix a/b/c)) -> (Ok .))
    (((path ./file) (prefix .)) -> (Ok file))
    (((path dir/.) (prefix dir)) -> (Ok .))
    (((path ../..) (prefix ..)) -> (Ok ..))
    (((path a/b/c/d) (prefix a)) -> (Ok b/c/d))
    (((path a/b/c/d) (prefix a/b)) -> (Ok c/d))
    (((path a/b/c/d) (prefix a/b/c)) -> (Ok d))
    (((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (prefix /)) -> (Ok .))
    (((path /..) (prefix /.))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /..) (prefix /.)))))
    (((path /./b) (prefix /./a))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /./b) (prefix /./a)))))
    (((path /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /c/d) (prefix /a/b)))))
    (((path /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /a) (prefix /a/b/c)))))
    (((path /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path /a/b) (prefix /a/b/c)))))
    (((path /.) (prefix /.)) -> (Ok .))
    (((path /..) (prefix /..)) -> (Ok .))
    (((path /a/b/c) (prefix /a/b/c)) -> (Ok .))
    (((path /./file) (prefix /.)) -> (Ok file))
    (((path /dir/.) (prefix /dir)) -> (Ok .))
    (((path /../..) (prefix /..)) -> (Ok ..))
    (((path /a/b/c/d) (prefix /a)) -> (Ok b/c/d))
    (((path /a/b/c/d) (prefix /a/b)) -> (Ok c/d))
    (((path /a/b/c/d) (prefix /a/b/c)) -> (Ok d))
    (((path /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (prefix .))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path /) (prefix .)))))
    (((path .) (prefix /))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix" ((path .) (prefix /)))))
    (((path /a/b/c) (prefix a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path /a/b/c) (prefix a/b)))))
    (((path a/b/c) (prefix /a/b))
     ->
     (Error
      ("File_path.chop_prefix_or_error: not a prefix"
       ((path a/b/c) (prefix /a/b)))))
    |}]
;;

let chop_prefix_if_exists = (File_path.chop_prefix_if_exists [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_if_exists
    (chop_prefix_if_exists [@alloc stack])
    (module Fn_labelled (With_prefix (File_path)) (File_path))
    ~examples:Examples.Path.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_if_exists ->
      require_equal
        (module File_path)
        (chop_prefix path ~prefix
         |> Option.value_map ~f:File_path.of_relative ~default:path)
        chop_prefix_if_exists
        ~message:"[chop_prefix_if_exists] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path ..) (prefix .)) -> ..)
    (((path ./b) (prefix ./a)) -> ./b)
    (((path c/d) (prefix a/b)) -> c/d)
    (((path a) (prefix a/b/c)) -> a)
    (((path a/b) (prefix a/b/c)) -> a/b)
    (((path .) (prefix .)) -> .)
    (((path ..) (prefix ..)) -> .)
    (((path a/b/c) (prefix a/b/c)) -> .)
    (((path ./file) (prefix .)) -> file)
    (((path dir/.) (prefix dir)) -> .)
    (((path ../..) (prefix ..)) -> ..)
    (((path a/b/c/d) (prefix a)) -> b/c/d)
    (((path a/b/c/d) (prefix a/b)) -> c/d)
    (((path a/b/c/d) (prefix a/b/c)) -> d)
    (((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))
     ->
     ending/in/this)
    (((path /) (prefix /)) -> .)
    (((path /..) (prefix /.)) -> /..)
    (((path /./b) (prefix /./a)) -> /./b)
    (((path /c/d) (prefix /a/b)) -> /c/d)
    (((path /a) (prefix /a/b/c)) -> /a)
    (((path /a/b) (prefix /a/b/c)) -> /a/b)
    (((path /.) (prefix /.)) -> .)
    (((path /..) (prefix /..)) -> .)
    (((path /a/b/c) (prefix /a/b/c)) -> .)
    (((path /./file) (prefix /.)) -> file)
    (((path /dir/.) (prefix /dir)) -> .)
    (((path /../..) (prefix /..)) -> ..)
    (((path /a/b/c/d) (prefix /a)) -> b/c/d)
    (((path /a/b/c/d) (prefix /a/b)) -> c/d)
    (((path /a/b/c/d) (prefix /a/b/c)) -> d)
    (((path /long/chain/of/names/ending/in/this) (prefix /long/chain/of/names))
     ->
     ending/in/this)
    (((path /) (prefix .)) -> /)
    (((path .) (prefix /)) -> .)
    (((path /a/b/c) (prefix a/b)) -> /a/b/c)
    (((path a/b/c) (prefix /a/b)) -> a/b/c)
    |}]
;;

let is_suffix = File_path.is_suffix

let%expect_test _ =
  test_predicate
    is_suffix
    (module Fn_labelled (With_suffix_local (File_path)) (Bool))
    ~examples:Examples.Path.for_chop_suffix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((path .) (suffix .))
       ((path ..) (suffix ..))
       ((path a/b/c) (suffix a/b/c))
       ((path ./file) (suffix file))
       ((path dir/.) (suffix .))
       ((path ../..) (suffix ..))
       ((path a/b/c/d) (suffix b/c/d))
       ((path a/b/c/d) (suffix c/d))
       ((path a/b/c/d) (suffix d))
       ((path long/chain/of/names/ending/in/this) (suffix ending/in/this))
       ((path /.) (suffix .))
       ((path /..) (suffix ..))
       ((path /a/b/c) (suffix a/b/c))
       ((path /./file) (suffix file))
       ((path /dir/.) (suffix .))
       ((path /../..) (suffix ..))
       ((path /a/b/c/d) (suffix b/c/d))
       ((path /a/b/c/d) (suffix c/d))
       ((path /a/b/c/d) (suffix d))
       ((path /long/chain/of/names/ending/in/this) (suffix ending/in/this))))
     (failure
      (((path ..) (suffix .))
       ((path b/.) (suffix a/.))
       ((path c/d) (suffix a/b))
       ((path c) (suffix a/b/c))
       ((path b/c) (suffix a/b/c))
       ((path /) (suffix .))
       ((path /..) (suffix .))
       ((path /b/.) (suffix a/.))
       ((path /c/d) (suffix a/b))
       ((path /c) (suffix a/b/c))
       ((path /b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = (File_path.chop_suffix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_suffix
    (chop_suffix [@alloc stack])
    (module Fn_labelled (With_suffix (File_path)) (Option_of (File_path)))
    ~examples:Examples.Path.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix ->
      require_equal
        (module Bool)
        (is_suffix path ~suffix)
        (Option.is_some chop_suffix)
        ~message:"[chop_suffix] and [is_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .)) -> ())
    (((path b/.) (suffix a/.)) -> ())
    (((path c/d) (suffix a/b)) -> ())
    (((path c) (suffix a/b/c)) -> ())
    (((path b/c) (suffix a/b/c)) -> ())
    (((path .) (suffix .)) -> (.))
    (((path ..) (suffix ..)) -> (.))
    (((path a/b/c) (suffix a/b/c)) -> (.))
    (((path ./file) (suffix file)) -> (.))
    (((path dir/.) (suffix .)) -> (dir))
    (((path ../..) (suffix ..)) -> (..))
    (((path a/b/c/d) (suffix b/c/d)) -> (a))
    (((path a/b/c/d) (suffix c/d)) -> (a/b))
    (((path a/b/c/d) (suffix d)) -> (a/b/c))
    (((path long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (long/chain/of/names))
    (((path /) (suffix .)) -> ())
    (((path /..) (suffix .)) -> ())
    (((path /b/.) (suffix a/.)) -> ())
    (((path /c/d) (suffix a/b)) -> ())
    (((path /c) (suffix a/b/c)) -> ())
    (((path /b/c) (suffix a/b/c)) -> ())
    (((path /.) (suffix .)) -> (/))
    (((path /..) (suffix ..)) -> (/))
    (((path /a/b/c) (suffix a/b/c)) -> (/))
    (((path /./file) (suffix file)) -> (/.))
    (((path /dir/.) (suffix .)) -> (/dir))
    (((path /../..) (suffix ..)) -> (/..))
    (((path /a/b/c/d) (suffix b/c/d)) -> (/a))
    (((path /a/b/c/d) (suffix c/d)) -> (/a/b))
    (((path /a/b/c/d) (suffix d)) -> (/a/b/c))
    (((path /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (/long/chain/of/names))
    |}]
;;

let chop_suffix_exn = (File_path.chop_suffix_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_suffix_exn
    (chop_suffix_exn [@alloc stack])
    (module Fn_labelled_exn (With_suffix (File_path)) (File_path))
    ~examples:Examples.Path.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_exn ->
      require_equal
        (module Option_of (File_path))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path ..) (suffix .)))))
    (((path b/.) (suffix a/.))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path b/.) (suffix a/.)))))
    (((path c/d) (suffix a/b))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path c/d) (suffix a/b)))))
    (((path c) (suffix a/b/c))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path c) (suffix a/b/c)))))
    (((path b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path b/c) (suffix a/b/c)))))
    (((path .) (suffix .)) -> (Ok .))
    (((path ..) (suffix ..)) -> (Ok .))
    (((path a/b/c) (suffix a/b/c)) -> (Ok .))
    (((path ./file) (suffix file)) -> (Ok .))
    (((path dir/.) (suffix .)) -> (Ok dir))
    (((path ../..) (suffix ..)) -> (Ok ..))
    (((path a/b/c/d) (suffix b/c/d)) -> (Ok a))
    (((path a/b/c/d) (suffix c/d)) -> (Ok a/b))
    (((path a/b/c/d) (suffix d)) -> (Ok a/b/c))
    (((path long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok long/chain/of/names))
    (((path /) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path /) (suffix .)))))
    (((path /..) (suffix .))
     ->
     (Error ("File_path.chop_suffix_exn: not a suffix" ((path /..) (suffix .)))))
    (((path /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /b/.) (suffix a/.)))))
    (((path /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /c/d) (suffix a/b)))))
    (((path /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /c) (suffix a/b/c)))))
    (((path /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_exn: not a suffix" ((path /b/c) (suffix a/b/c)))))
    (((path /.) (suffix .)) -> (Ok /))
    (((path /..) (suffix ..)) -> (Ok /))
    (((path /a/b/c) (suffix a/b/c)) -> (Ok /))
    (((path /./file) (suffix file)) -> (Ok /.))
    (((path /dir/.) (suffix .)) -> (Ok /dir))
    (((path /../..) (suffix ..)) -> (Ok /..))
    (((path /a/b/c/d) (suffix b/c/d)) -> (Ok /a))
    (((path /a/b/c/d) (suffix c/d)) -> (Ok /a/b))
    (((path /a/b/c/d) (suffix d)) -> (Ok /a/b/c))
    (((path /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok /long/chain/of/names))
    |}]
;;

let chop_suffix_or_error = (File_path.chop_suffix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_or_error
    (chop_suffix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_suffix (File_path)) (File_path))
    ~examples:Examples.Path.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_or_error ->
      require_equal
        (module Option_of (File_path))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path ..) (suffix .)))))
    (((path b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path b/.) (suffix a/.)))))
    (((path c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path c/d) (suffix a/b)))))
    (((path c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path c) (suffix a/b/c)))))
    (((path b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path b/c) (suffix a/b/c)))))
    (((path .) (suffix .)) -> (Ok .))
    (((path ..) (suffix ..)) -> (Ok .))
    (((path a/b/c) (suffix a/b/c)) -> (Ok .))
    (((path ./file) (suffix file)) -> (Ok .))
    (((path dir/.) (suffix .)) -> (Ok dir))
    (((path ../..) (suffix ..)) -> (Ok ..))
    (((path a/b/c/d) (suffix b/c/d)) -> (Ok a))
    (((path a/b/c/d) (suffix c/d)) -> (Ok a/b))
    (((path a/b/c/d) (suffix d)) -> (Ok a/b/c))
    (((path long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok long/chain/of/names))
    (((path /) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /) (suffix .)))))
    (((path /..) (suffix .))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /..) (suffix .)))))
    (((path /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /b/.) (suffix a/.)))))
    (((path /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /c/d) (suffix a/b)))))
    (((path /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix" ((path /c) (suffix a/b/c)))))
    (((path /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.chop_suffix_or_error: not a suffix"
       ((path /b/c) (suffix a/b/c)))))
    (((path /.) (suffix .)) -> (Ok /))
    (((path /..) (suffix ..)) -> (Ok /))
    (((path /a/b/c) (suffix a/b/c)) -> (Ok /))
    (((path /./file) (suffix file)) -> (Ok /.))
    (((path /dir/.) (suffix .)) -> (Ok /dir))
    (((path /../..) (suffix ..)) -> (Ok /..))
    (((path /a/b/c/d) (suffix b/c/d)) -> (Ok /a))
    (((path /a/b/c/d) (suffix c/d)) -> (Ok /a/b))
    (((path /a/b/c/d) (suffix d)) -> (Ok /a/b/c))
    (((path /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     (Ok /long/chain/of/names))
    |}]
;;

let chop_suffix_if_exists = (File_path.chop_suffix_if_exists [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_if_exists
    (chop_suffix_if_exists [@alloc stack])
    (module Fn_labelled (With_suffix (File_path)) (File_path))
    ~examples:Examples.Path.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path)
        (chop_suffix path ~suffix |> Option.value ~default:path)
        chop_suffix_if_exists
        ~message:"[chop_suffix_if_exists] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .)) -> ..)
    (((path b/.) (suffix a/.)) -> b/.)
    (((path c/d) (suffix a/b)) -> c/d)
    (((path c) (suffix a/b/c)) -> c)
    (((path b/c) (suffix a/b/c)) -> b/c)
    (((path .) (suffix .)) -> .)
    (((path ..) (suffix ..)) -> .)
    (((path a/b/c) (suffix a/b/c)) -> .)
    (((path ./file) (suffix file)) -> .)
    (((path dir/.) (suffix .)) -> dir)
    (((path ../..) (suffix ..)) -> ..)
    (((path a/b/c/d) (suffix b/c/d)) -> a)
    (((path a/b/c/d) (suffix c/d)) -> a/b)
    (((path a/b/c/d) (suffix d)) -> a/b/c)
    (((path long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     long/chain/of/names)
    (((path /) (suffix .)) -> /)
    (((path /..) (suffix .)) -> /..)
    (((path /b/.) (suffix a/.)) -> /b/.)
    (((path /c/d) (suffix a/b)) -> /c/d)
    (((path /c) (suffix a/b/c)) -> /c)
    (((path /b/c) (suffix a/b/c)) -> /b/c)
    (((path /.) (suffix .)) -> /)
    (((path /..) (suffix ..)) -> /)
    (((path /a/b/c) (suffix a/b/c)) -> /)
    (((path /./file) (suffix file)) -> /.)
    (((path /dir/.) (suffix .)) -> /dir)
    (((path /../..) (suffix ..)) -> /..)
    (((path /a/b/c/d) (suffix b/c/d)) -> /a)
    (((path /a/b/c/d) (suffix c/d)) -> /a/b)
    (((path /a/b/c/d) (suffix d)) -> /a/b/c)
    (((path /long/chain/of/names/ending/in/this) (suffix ending/in/this))
     ->
     /long/chain/of/names)
    |}]
;;

let append = (File_path.append [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append
    (append [@alloc stack])
    (module Fn2_local (File_path) (File_path.Relative) (File_path))
    ~examples:Examples.Path.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Option_of (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Option_of (File_path))
        (Some prefix)
        (chop_suffix append ~suffix)
        ~message:"[append] and [chop_suffix] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b/c/d) -> a/b/c/d)
    ((a/b c/d) -> a/b/c/d)
    ((a/b/c d) -> a/b/c/d)
    ((long/chain/of/names ending/in/this) -> long/chain/of/names/ending/in/this)
    ((/ .) -> /.)
    ((/ ..) -> /..)
    ((/ a/b/c) -> /a/b/c)
    ((/. file) -> /./file)
    ((/dir .) -> /dir/.)
    ((/.. ..) -> /../..)
    ((/a b/c/d) -> /a/b/c/d)
    ((/a/b c/d) -> /a/b/c/d)
    ((/a/b/c d) -> /a/b/c/d)
    ((/long/chain/of/names ending/in/this) -> /long/chain/of/names/ending/in/this)
    |}]
;;

let to_parts = (File_path.to_parts [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_parts
    (to_parts [@alloc stack])
    (module Fn (File_path) (List_of (File_path.Part)))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun path to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts path)
        ~message:"[to_parts] and [number_of_parts] are inconsistent");
  [%expect
    {|
    (. -> (.))
    (.. -> (..))
    (filename.txt -> (filename.txt))
    (bin -> (bin))
    (.hidden -> (.hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> ("\001\255"))
    (./. -> (. .))
    (../.. -> (.. ..))
    (././. -> (. . .))
    (bin/exe -> (bin exe))
    (bin/exe/file -> (bin exe file))
    (/ -> ())
    (/. -> (.))
    (/.. -> (..))
    (/filename.txt -> (filename.txt))
    (/bin -> (bin))
    (/.hidden -> (.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> ("\001\255"))
    (/./. -> (. .))
    (/../.. -> (.. ..))
    (/././. -> (. . .))
    (/bin/exe -> (bin exe))
    (/bin/exe/file -> (bin exe file))
    |}]
;;

let of_parts_absolute = (File_path.of_parts_absolute [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_absolute
    (of_parts_absolute [@alloc stack])
    (module Fn_local (List_of (File_path.Part)) (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_absolute ->
      require_equal
        (module List_of (File_path.Part))
        (to_parts of_parts_absolute)
        parts
        ~message:"[of_parts_absolute] and [to_parts] are inconsistent");
  [%expect
    {|
    (() -> /)
    ((.) -> /.)
    ((..) -> /..)
    ((filename.txt) -> /filename.txt)
    ((bin) -> /bin)
    ((.hidden) -> /.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "/\001\255")
    ((. .) -> /./.)
    ((.. .) -> /../.)
    ((.. .) -> /../.)
    ((.. ..) -> /../..)
    ((filename.txt .) -> /filename.txt/.)
    ((.. filename.txt) -> /../filename.txt)
    ((bin .) -> /bin/.)
    ((.. bin) -> /../bin)
    ((.hidden .) -> /.hidden/.)
    ((.. .hidden) -> /../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "/\001\255/.")
    ((.. "\001\255") -> "/../\001\255")
    ((.hidden bin.exe) -> /.hidden/bin.exe)
    ((.hidden bin exe.file) -> /.hidden/bin/exe.file)
    |}]
;;

let of_parts_relative = (File_path.of_parts_relative [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_relative
    (of_parts_relative [@alloc stack])
    (module Fn (List_of (File_path.Part)) (Option_of (File_path)))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative ->
      require_equal
        (module List_of (File_path.Part))
        (Option.value_map of_parts_relative ~f:to_parts ~default:[])
        parts
        ~message:"[of_parts_relative] and [to_parts] are inconsistent");
  [%expect
    {|
    (() -> ())
    ((.) -> (.))
    ((..) -> (..))
    ((filename.txt) -> (filename.txt))
    ((bin) -> (bin))
    ((.hidden) -> (.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> ("\001\255"))
    ((. .) -> (./.))
    ((.. .) -> (../.))
    ((.. .) -> (../.))
    ((.. ..) -> (../..))
    ((filename.txt .) -> (filename.txt/.))
    ((.. filename.txt) -> (../filename.txt))
    ((bin .) -> (bin/.))
    ((.. bin) -> (../bin))
    ((.hidden .) -> (.hidden/.))
    ((.. .hidden) -> (../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     ("This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     ("../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> ("\001\255/."))
    ((.. "\001\255") -> ("../\001\255"))
    ((.hidden bin.exe) -> (.hidden/bin.exe))
    ((.hidden bin exe.file) -> (.hidden/bin/exe.file))
    |}]
;;

let of_parts_relative_exn = (File_path.of_parts_relative_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_relative_exn
    (of_parts_relative_exn [@alloc stack])
    (module Fn_exn (List_of (File_path.Part)) (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_exn ->
      require_equal
        (module Option_of (File_path))
        (of_parts_relative parts)
        (Or_error.ok of_parts_relative_exn)
        ~message:"[of_parts_relative_exn] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.of_parts_relative_exn: empty list"))
    ((.) -> (Ok .))
    ((..) -> (Ok ..))
    ((filename.txt) -> (Ok filename.txt))
    ((bin) -> (Ok bin))
    ((.hidden) -> (Ok .hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> (Ok "\001\255"))
    ((. .) -> (Ok ./.))
    ((.. .) -> (Ok ../.))
    ((.. .) -> (Ok ../.))
    ((.. ..) -> (Ok ../..))
    ((filename.txt .) -> (Ok filename.txt/.))
    ((.. filename.txt) -> (Ok ../filename.txt))
    ((bin .) -> (Ok bin/.))
    ((.. bin) -> (Ok ../bin))
    ((.hidden .) -> (Ok .hidden/.))
    ((.. .hidden) -> (Ok ../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> (Ok "\001\255/."))
    ((.. "\001\255") -> (Ok "../\001\255"))
    ((.hidden bin.exe) -> (Ok .hidden/bin.exe))
    ((.hidden bin exe.file) -> (Ok .hidden/bin/exe.file))
    |}]
;;

let of_parts_relative_or_error = (File_path.of_parts_relative_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_relative_or_error
    (of_parts_relative_or_error [@alloc stack])
    (module Fn_or_error (List_of (File_path.Part)) (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_or_error ->
      require_equal
        (module Option_of (File_path))
        (of_parts_relative parts)
        (Or_error.ok of_parts_relative_or_error)
        ~message:"[of_parts_relative_or_error] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.of_parts_relative_or_error: empty list"))
    ((.) -> (Ok .))
    ((..) -> (Ok ..))
    ((filename.txt) -> (Ok filename.txt))
    ((bin) -> (Ok bin))
    ((.hidden) -> (Ok .hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255") -> (Ok "\001\255"))
    ((. .) -> (Ok ./.))
    ((.. .) -> (Ok ../.))
    ((.. .) -> (Ok ../.))
    ((.. ..) -> (Ok ../..))
    ((filename.txt .) -> (Ok filename.txt/.))
    ((.. filename.txt) -> (Ok ../filename.txt))
    ((bin .) -> (Ok bin/.))
    ((.. bin) -> (Ok ../bin))
    ((.hidden .) -> (Ok .hidden/.))
    ((.. .hidden) -> (Ok ../.hidden))
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     (Ok "This is a sentence; it has punctuation, capitalization, and spaces!/."))
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     (Ok "../This is a sentence; it has punctuation, capitalization, and spaces!"))
    (("\001\255" .) -> (Ok "\001\255/."))
    ((.. "\001\255") -> (Ok "../\001\255"))
    ((.hidden bin.exe) -> (Ok .hidden/bin.exe))
    ((.hidden bin exe.file) -> (Ok .hidden/bin/exe.file))
    |}]
;;

let of_parts_relative_defaulting_to_dot =
  (File_path.of_parts_relative_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_relative_defaulting_to_dot
    (of_parts_relative_defaulting_to_dot [@alloc stack])
    (module Fn (List_of (File_path.Part)) (File_path))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_relative_defaulting_to_dot ->
      require_equal
        (module File_path)
        (Option.value (of_parts_relative parts) ~default:dot)
        of_parts_relative_defaulting_to_dot
        ~message:
          "[of_parts_relative_defaulting_to_dot] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    (() -> .)
    ((.) -> .)
    ((..) -> ..)
    ((filename.txt) -> filename.txt)
    ((bin) -> bin)
    ((.hidden) -> .hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "\001\255")
    ((. .) -> ./.)
    ((.. .) -> ../.)
    ((.. .) -> ../.)
    ((.. ..) -> ../..)
    ((filename.txt .) -> filename.txt/.)
    ((.. filename.txt) -> ../filename.txt)
    ((bin .) -> bin/.)
    ((.. bin) -> ../bin)
    ((.hidden .) -> .hidden/.)
    ((.. .hidden) -> ../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "\001\255/.")
    ((.. "\001\255") -> "../\001\255")
    ((.hidden bin.exe) -> .hidden/bin.exe)
    ((.hidden bin exe.file) -> .hidden/bin/exe.file)
    |}]
;;

let of_parts_relative_nonempty = (File_path.of_parts_relative_nonempty [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_relative_nonempty
    (of_parts_relative_nonempty [@alloc stack])
    (module Fn (Nonempty_list_of (File_path.Part)) (File_path))
    ~examples:
      (List.filter_map Examples.Part.lists_for_conversion ~f:Nonempty_list.of_list)
    ~correctness:(fun parts of_parts_relative_nonempty ->
      require_equal
        (module Option_of (File_path))
        (of_parts_relative (Nonempty_list.to_list parts))
        (Some of_parts_relative_nonempty)
        ~message:"[of_parts_relative_nonempty] and [of_parts_relative] are inconsistent");
  [%expect
    {|
    ((.) -> .)
    ((..) -> ..)
    ((filename.txt) -> filename.txt)
    ((bin) -> bin)
    ((.hidden) -> .hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255") -> "\001\255")
    ((. .) -> ./.)
    ((.. .) -> ../.)
    ((.. .) -> ../.)
    ((.. ..) -> ../..)
    ((filename.txt .) -> filename.txt/.)
    ((.. filename.txt) -> ../filename.txt)
    ((bin .) -> bin/.)
    ((.. bin) -> ../bin)
    ((.hidden .) -> .hidden/.)
    ((.. .hidden) -> ../.hidden)
    (("This is a sentence; it has punctuation, capitalization, and spaces!" .)
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!/.")
    ((.. "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "../This is a sentence; it has punctuation, capitalization, and spaces!")
    (("\001\255" .) -> "\001\255/.")
    ((.. "\001\255") -> "../\001\255")
    ((.hidden bin.exe) -> .hidden/bin.exe)
    ((.hidden bin exe.file) -> .hidden/bin/exe.file)
    |}]
;;

let make_absolute = (File_path.make_absolute [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    make_absolute
    (make_absolute [@alloc stack])
    (module Fn_labelled (With_under (File_path)) (File_path.Absolute))
    ~examples:Examples.Path.for_make_absolute
    ~correctness:(fun { path; under } make_absolute ->
      if is_absolute path
      then
        require_equal
          (module File_path.Absolute)
          (to_absolute_exn path)
          make_absolute
          ~message:"[make_absolute] and [to_absolute_exn] are inconsistent"
      else
        require_equal
          (module Option_of (File_path.Relative))
          (Some (to_relative_exn path))
          (File_path.Absolute.chop_prefix make_absolute ~prefix:under)
          ~message:"[make_absolute] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path .) (under /)) -> /.)
    (((path ..) (under /)) -> /..)
    (((path a/b/c) (under /)) -> /a/b/c)
    (((path file) (under /.)) -> /./file)
    (((path .) (under /dir)) -> /dir/.)
    (((path ..) (under /..)) -> /../..)
    (((path b/c/d) (under /a)) -> /a/b/c/d)
    (((path c/d) (under /a/b)) -> /a/b/c/d)
    (((path d) (under /a/b/c)) -> /a/b/c/d)
    (((path ending/in/this) (under /long/chain/of/names))
     ->
     /long/chain/of/names/ending/in/this)
    (((path /) (under /.)) -> /)
    (((path /.) (under /)) -> /.)
    (((path /..) (under /)) -> /..)
    (((path /a/b/c) (under /)) -> /a/b/c)
    (((path /file) (under /.)) -> /file)
    (((path /.) (under /dir)) -> /.)
    (((path /..) (under /..)) -> /..)
    (((path /b/c/d) (under /a)) -> /b/c/d)
    (((path /c/d) (under /a/b)) -> /c/d)
    (((path /d) (under /a/b/c)) -> /d)
    (((path /ending/in/this) (under /long/chain/of/names)) -> /ending/in/this)
    |}]
;;

let make_relative = (File_path.make_relative [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    make_relative
    (make_relative [@alloc stack])
    (module Fn_labelled (With_if_under (File_path)) (Option_of (File_path.Relative)))
    ~examples:Examples.Path.for_make_relative
    ~correctness:(fun { path; if_under } make_relative ->
      require_equal
        (module Option_of (File_path.Relative))
        make_relative
        (if is_relative path
         then Some (to_relative_exn path)
         else chop_prefix path ~prefix:(of_absolute if_under))
        ~message:"[make_relative] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path .) (if_under /)) -> (.))
    (((path ..) (if_under /)) -> (..))
    (((path a/b/c) (if_under /)) -> (a/b/c))
    (((path file) (if_under /.)) -> (file))
    (((path .) (if_under /dir)) -> (.))
    (((path ..) (if_under /..)) -> (..))
    (((path b/c/d) (if_under /a)) -> (b/c/d))
    (((path c/d) (if_under /a/b)) -> (c/d))
    (((path d) (if_under /a/b/c)) -> (d))
    (((path ending/in/this) (if_under /long/chain/of/names)) -> (ending/in/this))
    (((path /) (if_under /)) -> (.))
    (((path /..) (if_under /.)) -> ())
    (((path /./b) (if_under /./a)) -> ())
    (((path /c/d) (if_under /a/b)) -> ())
    (((path /a) (if_under /a/b/c)) -> ())
    (((path /a/b) (if_under /a/b/c)) -> ())
    (((path /.) (if_under /.)) -> (.))
    (((path /..) (if_under /..)) -> (.))
    (((path /a/b/c) (if_under /a/b/c)) -> (.))
    (((path /./file) (if_under /.)) -> (file))
    (((path /dir/.) (if_under /dir)) -> (.))
    (((path /../..) (if_under /..)) -> (..))
    (((path /a/b/c/d) (if_under /a)) -> (b/c/d))
    (((path /a/b/c/d) (if_under /a/b)) -> (c/d))
    (((path /a/b/c/d) (if_under /a/b/c)) -> (d))
    (((path /long/chain/of/names/ending/in/this) (if_under /long/chain/of/names))
     ->
     (ending/in/this))
    |}]
;;

let make_relative_exn = (File_path.make_relative_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    make_relative_exn
    (make_relative_exn [@alloc stack])
    (module Fn_labelled_exn (With_if_under (File_path)) (File_path.Relative))
    ~examples:Examples.Path.for_make_relative
    ~correctness:(fun { path; if_under } make_relative_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok make_relative_exn)
        (make_relative path ~if_under)
        ~message:"[make_relative_exn] and [make_relative] are inconsistent");
  [%expect
    {|
    (((path .) (if_under /)) -> (Ok .))
    (((path ..) (if_under /)) -> (Ok ..))
    (((path a/b/c) (if_under /)) -> (Ok a/b/c))
    (((path file) (if_under /.)) -> (Ok file))
    (((path .) (if_under /dir)) -> (Ok .))
    (((path ..) (if_under /..)) -> (Ok ..))
    (((path b/c/d) (if_under /a)) -> (Ok b/c/d))
    (((path c/d) (if_under /a/b)) -> (Ok c/d))
    (((path d) (if_under /a/b/c)) -> (Ok d))
    (((path ending/in/this) (if_under /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (if_under /)) -> (Ok .))
    (((path /..) (if_under /.))
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /..) (if_under /.)))))
    (((path /./b) (if_under /./a))
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /./b) (if_under /./a)))))
    (((path /c/d) (if_under /a/b))
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /c/d) (if_under /a/b)))))
    (((path /a) (if_under /a/b/c))
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /a) (if_under /a/b/c)))))
    (((path /a/b) (if_under /a/b/c))
     ->
     (Error
      ("File_path.make_relative_exn: cannot make path relative"
       ((path /a/b) (if_under /a/b/c)))))
    (((path /.) (if_under /.)) -> (Ok .))
    (((path /..) (if_under /..)) -> (Ok .))
    (((path /a/b/c) (if_under /a/b/c)) -> (Ok .))
    (((path /./file) (if_under /.)) -> (Ok file))
    (((path /dir/.) (if_under /dir)) -> (Ok .))
    (((path /../..) (if_under /..)) -> (Ok ..))
    (((path /a/b/c/d) (if_under /a)) -> (Ok b/c/d))
    (((path /a/b/c/d) (if_under /a/b)) -> (Ok c/d))
    (((path /a/b/c/d) (if_under /a/b/c)) -> (Ok d))
    (((path /long/chain/of/names/ending/in/this) (if_under /long/chain/of/names))
     ->
     (Ok ending/in/this))
    |}]
;;

let make_relative_or_error = (File_path.make_relative_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    make_relative_or_error
    (make_relative_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_if_under (File_path)) (File_path.Relative))
    ~examples:Examples.Path.for_make_relative
    ~correctness:(fun { path; if_under } make_relative_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok make_relative_or_error)
        (make_relative path ~if_under)
        ~message:"[make_relative_or_error] and [make_relative] are inconsistent");
  [%expect
    {|
    (((path .) (if_under /)) -> (Ok .))
    (((path ..) (if_under /)) -> (Ok ..))
    (((path a/b/c) (if_under /)) -> (Ok a/b/c))
    (((path file) (if_under /.)) -> (Ok file))
    (((path .) (if_under /dir)) -> (Ok .))
    (((path ..) (if_under /..)) -> (Ok ..))
    (((path b/c/d) (if_under /a)) -> (Ok b/c/d))
    (((path c/d) (if_under /a/b)) -> (Ok c/d))
    (((path d) (if_under /a/b/c)) -> (Ok d))
    (((path ending/in/this) (if_under /long/chain/of/names))
     ->
     (Ok ending/in/this))
    (((path /) (if_under /)) -> (Ok .))
    (((path /..) (if_under /.))
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /..) (if_under /.)))))
    (((path /./b) (if_under /./a))
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /./b) (if_under /./a)))))
    (((path /c/d) (if_under /a/b))
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /c/d) (if_under /a/b)))))
    (((path /a) (if_under /a/b/c))
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /a) (if_under /a/b/c)))))
    (((path /a/b) (if_under /a/b/c))
     ->
     (Error
      ("File_path.make_relative_or_error: cannot make path relative"
       ((path /a/b) (if_under /a/b/c)))))
    (((path /.) (if_under /.)) -> (Ok .))
    (((path /..) (if_under /..)) -> (Ok .))
    (((path /a/b/c) (if_under /a/b/c)) -> (Ok .))
    (((path /./file) (if_under /.)) -> (Ok file))
    (((path /dir/.) (if_under /dir)) -> (Ok .))
    (((path /../..) (if_under /..)) -> (Ok ..))
    (((path /a/b/c/d) (if_under /a)) -> (Ok b/c/d))
    (((path /a/b/c/d) (if_under /a/b)) -> (Ok c/d))
    (((path /a/b/c/d) (if_under /a/b/c)) -> (Ok d))
    (((path /long/chain/of/names/ending/in/this) (if_under /long/chain/of/names))
     ->
     (Ok ending/in/this))
    |}]
;;

let make_relative_if_possible = (File_path.make_relative_if_possible [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    make_relative_if_possible
    (make_relative_if_possible [@alloc stack])
    (module Fn_labelled (With_if_under (File_path)) (File_path))
    ~examples:Examples.Path.for_make_relative
    ~correctness:(fun { path; if_under } make_relative_if_possible ->
      require_equal
        (module File_path)
        make_relative_if_possible
        (Option.value_map (make_relative path ~if_under) ~f:of_relative ~default:path)
        ~message:"[make_relative_if_possible] and [make_relative] are inconsistent");
  [%expect
    {|
    (((path .) (if_under /)) -> .)
    (((path ..) (if_under /)) -> ..)
    (((path a/b/c) (if_under /)) -> a/b/c)
    (((path file) (if_under /.)) -> file)
    (((path .) (if_under /dir)) -> .)
    (((path ..) (if_under /..)) -> ..)
    (((path b/c/d) (if_under /a)) -> b/c/d)
    (((path c/d) (if_under /a/b)) -> c/d)
    (((path d) (if_under /a/b/c)) -> d)
    (((path ending/in/this) (if_under /long/chain/of/names)) -> ending/in/this)
    (((path /) (if_under /)) -> .)
    (((path /..) (if_under /.)) -> /..)
    (((path /./b) (if_under /./a)) -> /./b)
    (((path /c/d) (if_under /a/b)) -> /c/d)
    (((path /a) (if_under /a/b/c)) -> /a)
    (((path /a/b) (if_under /a/b/c)) -> /a/b)
    (((path /.) (if_under /.)) -> .)
    (((path /..) (if_under /..)) -> .)
    (((path /a/b/c) (if_under /a/b/c)) -> .)
    (((path /./file) (if_under /.)) -> file)
    (((path /dir/.) (if_under /dir)) -> .)
    (((path /../..) (if_under /..)) -> ..)
    (((path /a/b/c/d) (if_under /a)) -> b/c/d)
    (((path /a/b/c/d) (if_under /a/b)) -> c/d)
    (((path /a/b/c/d) (if_under /a/b/c)) -> d)
    (((path /long/chain/of/names/ending/in/this) (if_under /long/chain/of/names))
     ->
     ending/in/this)
    |}]
;;

module Variant = File_path.Variant

let to_variant = (File_path.to_variant [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_variant
    (to_variant [@alloc stack])
    (module Fn (File_path) (File_path.Variant))
    ~examples:Examples.Path.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    (. -> (Relative .))
    (.. -> (Relative ..))
    (filename.txt -> (Relative filename.txt))
    (bin -> (Relative bin))
    (.hidden -> (Relative .hidden))
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Relative
      "This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("\001\255" -> (Relative "\001\255"))
    (./. -> (Relative ./.))
    (../.. -> (Relative ../..))
    (././. -> (Relative ././.))
    (bin/exe -> (Relative bin/exe))
    (bin/exe/file -> (Relative bin/exe/file))
    (/ -> (Absolute /))
    (/. -> (Absolute /.))
    (/.. -> (Absolute /..))
    (/filename.txt -> (Absolute /filename.txt))
    (/bin -> (Absolute /bin))
    (/.hidden -> (Absolute /.hidden))
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     (Absolute
      "/This is a sentence; it has punctuation, capitalization, and spaces!"))
    ("/\001\255" -> (Absolute "/\001\255"))
    (/./. -> (Absolute /./.))
    (/../.. -> (Absolute /../..))
    (/././. -> (Absolute /././.))
    (/bin/exe -> (Absolute /bin/exe))
    (/bin/exe/file -> (Absolute /bin/exe/file))
    |}]
;;

let of_variant = (File_path.of_variant [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    of_variant
    (of_variant [@mode local])
    (module Fn (Variant) (File_path))
    ~examples:Examples.Path.variant_for_conversion
    ~correctness:(fun variant of_variant ->
      require_equal
        (module Variant)
        variant
        (to_variant of_variant)
        ~message:"[of_variant] and [to_variant] are inconsistent");
  [%expect
    {|
    ((Relative .) -> .)
    ((Relative ..) -> ..)
    ((Relative filename.txt) -> filename.txt)
    ((Relative bin) -> bin)
    ((Relative .hidden) -> .hidden)
    ((Relative
      "This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ((Relative "\001\255") -> "\001\255")
    ((Relative ./.) -> ./.)
    ((Relative ../..) -> ../..)
    ((Relative ././.) -> ././.)
    ((Relative bin/exe) -> bin/exe)
    ((Relative bin/exe/file) -> bin/exe/file)
    ((Absolute /) -> /)
    ((Absolute /.) -> /.)
    ((Absolute /..) -> /..)
    ((Absolute /filename.txt) -> /filename.txt)
    ((Absolute /bin) -> /bin)
    ((Absolute /.hidden) -> /.hidden)
    ((Absolute
      "/This is a sentence; it has punctuation, capitalization, and spaces!")
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ((Absolute "/\001\255") -> "/\001\255")
    ((Absolute /./.) -> /./.)
    ((Absolute /../..) -> /../..)
    ((Absolute /././.) -> /././.)
    ((Absolute /bin/exe) -> /bin/exe)
    ((Absolute /bin/exe/file) -> /bin/exe/file)
    |}]
;;

let simplify_dot = (File_path.simplify_dot [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    simplify_dot
    (simplify_dot [@alloc stack])
    (module Fn (File_path) (File_path))
    ~examples:Examples.Path.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module File_path)
        simplified
        (let parts =
           to_parts original |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot)
         in
         if is_absolute original
         then of_parts_absolute parts
         else of_parts_relative_defaulting_to_dot parts)
        ~message:"[simplify_dot] is not equivalent to filtering out [.] parts";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot original) : t)));
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    (a/b -> a/b)
    (a/b/. -> a/b)
    (a/./b -> a/b)
    (./a/b -> a/b)
    (./a/./b/. -> a/b)
    (a/b/./. -> a/b)
    (a/././b -> a/b)
    (././a/b -> a/b)
    (././a/././b/./. -> a/b)
    (a/b/.. -> a/b/..)
    (a/../b -> a/../b)
    (../a/b -> ../a/b)
    (../a/../b/.. -> ../a/../b/..)
    (a/b/../.. -> a/b/../..)
    (a/../../b -> a/../../b)
    (../../a/b -> ../../a/b)
    (../../a/../../b/../.. -> ../../a/../../b/../..)
    (a/b/./.. -> a/b/..)
    (a/./../b -> a/../b)
    (./../a/b -> ../a/b)
    (./../a/./../b/./.. -> ../a/../b/..)
    (a/b/../. -> a/b/..)
    (a/.././b -> a/../b)
    (.././a/b -> ../a/b)
    (.././a/.././b/../. -> ../a/../b/..)
    (/ -> /)
    (/. -> /)
    (/.. -> /..)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/a/b -> /a/b)
    (/a/b/. -> /a/b)
    (/a/./b -> /a/b)
    (/./a/b -> /a/b)
    (/./a/./b/. -> /a/b)
    (/a/b/./. -> /a/b)
    (/a/././b -> /a/b)
    (/././a/b -> /a/b)
    (/././a/././b/./. -> /a/b)
    (/a/b/.. -> /a/b/..)
    (/a/../b -> /a/../b)
    (/../a/b -> /../a/b)
    (/../a/../b/.. -> /../a/../b/..)
    (/a/b/../.. -> /a/b/../..)
    (/a/../../b -> /a/../../b)
    (/../../a/b -> /../../a/b)
    (/../../a/../../b/../.. -> /../../a/../../b/../..)
    (/a/b/./.. -> /a/b/..)
    (/a/./../b -> /a/../b)
    (/./../a/b -> /../a/b)
    (/./../a/./../b/./.. -> /../a/../b/..)
    (/a/b/../. -> /a/b/..)
    (/a/.././b -> /a/../b)
    (/.././a/b -> /../a/b)
    (/.././a/.././b/../. -> /../a/../b/..)
    |}]
;;

let simplify_dot_and_dot_dot_naively =
  (File_path.simplify_dot_and_dot_dot_naively [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    simplify_dot_and_dot_dot_naively
    (simplify_dot_and_dot_dot_naively [@alloc stack])
    (module Fn (File_path) (File_path))
    ~examples:Examples.Path.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module List_of (File_path.Part))
        (to_parts simplified
         |> List.drop_while ~f:(File_path.Part.( = ) File_path.Part.dot_dot))
        (to_parts simplified
         |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot_dot))
        ~message:
          "not all [..] parts in [simplify_dot_and_dot_dot_naively] are at the beginning";
      require_equal
        (module File_path)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path)
        simplified
        (simplify_dot_and_dot_dot_naively simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] is not idempotent";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot_and_dot_dot_naively original) : t)));
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (filename.txt -> filename.txt)
    (bin -> bin)
    (.hidden -> .hidden)
    ("This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "This is a sentence; it has punctuation, capitalization, and spaces!")
    ("\001\255" -> "\001\255")
    (a/b -> a/b)
    (a/b/. -> a/b)
    (a/./b -> a/b)
    (./a/b -> a/b)
    (./a/./b/. -> a/b)
    (a/b/./. -> a/b)
    (a/././b -> a/b)
    (././a/b -> a/b)
    (././a/././b/./. -> a/b)
    (a/b/.. -> a)
    (a/../b -> b)
    (../a/b -> ../a/b)
    (../a/../b/.. -> ..)
    (a/b/../.. -> .)
    (a/../../b -> ../b)
    (../../a/b -> ../../a/b)
    (../../a/../../b/../.. -> ../../../..)
    (a/b/./.. -> a)
    (a/./../b -> b)
    (./../a/b -> ../a/b)
    (./../a/./../b/./.. -> ..)
    (a/b/../. -> a)
    (a/.././b -> b)
    (.././a/b -> ../a/b)
    (.././a/.././b/../. -> ..)
    (/ -> /)
    (/. -> /)
    (/.. -> /)
    (/filename.txt -> /filename.txt)
    (/bin -> /bin)
    (/.hidden -> /.hidden)
    ("/This is a sentence; it has punctuation, capitalization, and spaces!"
     ->
     "/This is a sentence; it has punctuation, capitalization, and spaces!")
    ("/\001\255" -> "/\001\255")
    (/a/b -> /a/b)
    (/a/b/. -> /a/b)
    (/a/./b -> /a/b)
    (/./a/b -> /a/b)
    (/./a/./b/. -> /a/b)
    (/a/b/./. -> /a/b)
    (/a/././b -> /a/b)
    (/././a/b -> /a/b)
    (/././a/././b/./. -> /a/b)
    (/a/b/.. -> /a)
    (/a/../b -> /b)
    (/../a/b -> /a/b)
    (/../a/../b/.. -> /)
    (/a/b/../.. -> /)
    (/a/../../b -> /b)
    (/../../a/b -> /a/b)
    (/../../a/../../b/../.. -> /)
    (/a/b/./.. -> /a)
    (/a/./../b -> /b)
    (/./../a/b -> /a/b)
    (/./../a/./../b/./.. -> /)
    (/a/b/../. -> /a)
    (/a/.././b -> /b)
    (/.././a/b -> /a/b)
    (/.././a/.././b/../. -> /)
    |}]
;;]
