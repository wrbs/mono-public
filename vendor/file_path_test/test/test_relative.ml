(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core
open Helpers

let dot = File_path.Relative.dot
let dot_dot = File_path.Relative.dot_dot

let%expect_test _ =
  test_constants (module File_path.Relative) [ dot; dot_dot ];
  [%expect
    {|
    .
    ..
    |}]
;;

open struct
  module Common = Test_common.Make (struct
      module Type = File_path.Types.Relative
      module Path = File_path.Relative
      module Examples = Examples.Relative
      module Tested = Test_relative_completion
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
    (~ ./ .)
    (~ .//. ./.)
    (~ .//.// ./.)
    (~ bin/exe/ bin/exe)
    (~ bin//exe//file bin/exe/file)
    (~ bin//exe//file/ bin/exe/file)
    (! ("File_path.Relative.of_string: invalid string" ""))
    (! ("File_path.Relative.of_string: invalid string" "invalid/\000/null"))
    (! ("File_path.Relative.of_string: invalid string" /invalid/absolute))

    Testing: containers
    (Set
     ("\001\255"
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
     (("\001\255" 0)
      (. 1)
      (./. 2)
      (././. 3)
      (.. 4)
      (../.. 5)
      (.hidden 6)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 7)
      (bin 8)
      (bin/exe 9)
      (bin/exe/file 10)
      (filename.txt 11)))
    (Hash_set
     ("\001\255"
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
     (("\001\255" 0)
      (. 1)
      (./. 2)
      (././. 3)
      (.. 4)
      (../.. 5)
      (.hidden 6)
      ("This is a sentence; it has punctuation, capitalization, and spaces!" 7)
      (bin 8)
      (bin/exe 9)
      (bin/exe/file 10)
      (filename.txt 11)))

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
    (! ("File_path.Relative.invariant: non-canonical representation" ./))
    (! ("File_path.Relative.invariant: non-canonical representation" .//.))
    (! ("File_path.Relative.invariant: non-canonical representation" .//.//))
    (! ("File_path.Relative.invariant: non-canonical representation" bin/exe/))
    (!
     ("File_path.Relative.invariant: non-canonical representation" bin//exe//file))
    (!
     ("File_path.Relative.invariant: non-canonical representation"
      bin//exe//file/))
    (! ("File_path.Relative.invariant: invalid string" ""))
    (! ("File_path.Relative.invariant: invalid string" "invalid/\000/null"))
    (! ("File_path.Relative.invariant: invalid string" /invalid/absolute))
    |}]
;;

[%%template
let number_of_parts = File_path.Relative.number_of_parts

let%expect_test _ =
  test_immediate
    number_of_parts
    (module Fn_local (File_path.Relative) (Int))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun _ number_of_parts ->
      require
        (Int.( >= ) number_of_parts 1)
        ~if_false_then_print_s:
          (lazy [%sexp "fewer than one part", { number_of_parts : int }]));
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
    |}]
;;

let of_part = (File_path.Relative.of_part [@mode l]) [@@mode l = (global, local)]

let%expect_test _ =
  test_function
    of_part
    (of_part [@mode local])
    (module Fn (File_path.Part) (File_path.Relative))
    ~examples:Examples.Part.for_conversion
    ~correctness:(fun _ of_part ->
      require_equal
        (module Int)
        (number_of_parts of_part)
        1
        ~message:"[of_part] and [number_of_parts] are inconsistent");
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

let basename = (File_path.Relative.basename [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    basename
    (basename [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Part))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
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
    |}]
;;

let dirname = (File_path.Relative.dirname [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname
    (dirname [@alloc stack])
    (module Fn (File_path.Relative) (Option_of (File_path.Relative)))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Int.equal (number_of_parts relative) 1)
        ~message:"[dirname] and [number_of_parts] are inconsistent");
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
    |}]
;;

let dirname_exn = (File_path.Relative.dirname_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname_exn
    (dirname_exn [@alloc stack])
    (module Fn_exn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok dirname_exn)
        (dirname relative)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.Relative.dirname_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.Relative.dirname_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.Relative.dirname_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    |}]
;;

let dirname_or_error = (File_path.Relative.dirname_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_or_error
    (dirname_or_error [@alloc stack])
    (module Fn_or_error (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok dirname_or_error)
        (dirname relative)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.dirname_or_error: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.dirname_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.dirname_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a/b))
    (a/b/c/d -> (Ok a/b/c))
    (long/chain/of/names/ending/in/this -> (Ok long/chain/of/names/ending/in))
    |}]
;;

let dirname_defaulting_to_dot = (File_path.Relative.dirname_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_defaulting_to_dot
    (dirname_defaulting_to_dot [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_basename_and_dirname
    ~correctness:(fun relative dirname_defaulting_to_dot ->
      require_equal
        (module File_path.Relative)
        dirname_defaulting_to_dot
        (Option.value (dirname relative) ~default:dot)
        ~message:"[dirname_defaulting_to_dot] and [dirname] are inconsistent");
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
    |}]
;;

let top_dir = (File_path.Relative.top_dir [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    top_dir
    (top_dir [@alloc stack])
    (module Fn (File_path.Relative) (Option_of (File_path.Part)))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir ->
      require_equal
        (module Bool)
        (Option.is_none top_dir)
        (Option.is_none (dirname relative))
        ~message:"[top_dir] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (.))
    (dir/. -> (dir))
    (../.. -> (..))
    (a/b -> (a))
    (a/b/c -> (a))
    (a/b/c/d -> (a))
    (long/chain/of/names/ending/in/this -> (long))
    |}]
;;

let top_dir_exn = (File_path.Relative.top_dir_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    top_dir_exn
    (top_dir_exn [@alloc stack])
    (module Fn_exn (File_path.Relative) (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_exn ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok top_dir_exn)
        (top_dir relative)
        ~message:"[top_dir_exn] and [top_dir] are inconsistent");
  [%expect
    {|
    (. -> (Error ("File_path.Relative.top_dir_exn: path contains no slash" .)))
    (.. -> (Error ("File_path.Relative.top_dir_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error ("File_path.Relative.top_dir_exn: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a))
    (a/b/c/d -> (Ok a))
    (long/chain/of/names/ending/in/this -> (Ok long))
    |}]
;;

let top_dir_or_error = (File_path.Relative.top_dir_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    top_dir_or_error
    (top_dir_or_error [@alloc stack])
    (module Fn_or_error (File_path.Relative) (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_or_error ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok top_dir_or_error)
        (top_dir relative)
        ~message:"[top_dir_or_error] and [top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.top_dir_or_error: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.top_dir_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.top_dir_or_error: path contains no slash" singleton)))
    (./file -> (Ok .))
    (dir/. -> (Ok dir))
    (../.. -> (Ok ..))
    (a/b -> (Ok a))
    (a/b/c -> (Ok a))
    (a/b/c/d -> (Ok a))
    (long/chain/of/names/ending/in/this -> (Ok long))
    |}]
;;

let top_dir_defaulting_to_dot = (File_path.Relative.top_dir_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    top_dir_defaulting_to_dot
    (top_dir_defaulting_to_dot [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Part))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative top_dir_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        top_dir_defaulting_to_dot
        (Option.value (top_dir relative) ~default:File_path.Part.dot)
        ~message:"[top_dir_defaulting_to_dot] and [top_dir] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> .)
    (singleton -> .)
    (./file -> .)
    (dir/. -> dir)
    (../.. -> ..)
    (a/b -> a)
    (a/b/c -> a)
    (a/b/c/d -> a)
    (long/chain/of/names/ending/in/this -> long)
    |}]
;;

let all_but_top_dir = (File_path.Relative.all_but_top_dir [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    all_but_top_dir
    (all_but_top_dir [@alloc stack])
    (module Fn (File_path.Relative) (Option_of (File_path.Relative)))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir ->
      require_equal
        (module Bool)
        (Option.is_none all_but_top_dir)
        (Option.is_none (dirname relative))
        ~message:"[all_but_top_dir] and [dirname] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> (file))
    (dir/. -> (.))
    (../.. -> (..))
    (a/b -> (b))
    (a/b/c -> (b/c))
    (a/b/c/d -> (b/c/d))
    (long/chain/of/names/ending/in/this -> (chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_exn = (File_path.Relative.all_but_top_dir_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    all_but_top_dir_exn
    (all_but_top_dir_exn [@alloc stack])
    (module Fn_exn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok all_but_top_dir_exn)
        (all_but_top_dir relative)
        ~message:"[all_but_top_dir_exn] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error ("File_path.Relative.all_but_top_dir_exn: path contains no slash" .)))
    (..
     ->
     (Error ("File_path.Relative.all_but_top_dir_exn: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_exn: path contains no slash" singleton)))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok b/c))
    (a/b/c/d -> (Ok b/c/d))
    (long/chain/of/names/ending/in/this -> (Ok chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_or_error = (File_path.Relative.all_but_top_dir_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    all_but_top_dir_or_error
    (all_but_top_dir_or_error [@alloc stack])
    (module Fn_or_error (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok all_but_top_dir_or_error)
        (all_but_top_dir relative)
        ~message:"[all_but_top_dir_or_error] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (.
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash" .)))
    (..
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash" ..)))
    (singleton
     ->
     (Error
      ("File_path.Relative.all_but_top_dir_or_error: path contains no slash"
       singleton)))
    (./file -> (Ok file))
    (dir/. -> (Ok .))
    (../.. -> (Ok ..))
    (a/b -> (Ok b))
    (a/b/c -> (Ok b/c))
    (a/b/c/d -> (Ok b/c/d))
    (long/chain/of/names/ending/in/this -> (Ok chain/of/names/ending/in/this))
    |}]
;;

let all_but_top_dir_defaulting_to_self =
  (File_path.Relative.all_but_top_dir_defaulting_to_self [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    all_but_top_dir_defaulting_to_self
    (all_but_top_dir_defaulting_to_self [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun relative all_but_top_dir_defaulting_to_self ->
      require_equal
        (module File_path.Relative)
        all_but_top_dir_defaulting_to_self
        (Option.value (all_but_top_dir relative) ~default:relative)
        ~message:
          "[all_but_top_dir_defaulting_to_self] and [all_but_top_dir] are inconsistent");
  [%expect
    {|
    (. -> .)
    (.. -> ..)
    (singleton -> singleton)
    (./file -> file)
    (dir/. -> .)
    (../.. -> ..)
    (a/b -> b)
    (a/b/c -> b/c)
    (a/b/c/d -> b/c/d)
    (long/chain/of/names/ending/in/this -> chain/of/names/ending/in/this)
    |}]
;;

let top_dir_and_all_but_top_dir =
  (File_path.Relative.top_dir_and_all_but_top_dir [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    top_dir_and_all_but_top_dir
    (top_dir_and_all_but_top_dir [@alloc stack])
    (module Fn
              (File_path.Relative)
              (Option_of (Pair_of (File_path.Part) (File_path.Relative))))
    ~examples:Examples.Relative.for_top_dir
    ~correctness:(fun path top_dir_and_all_but_top_dir ->
      require_equal
        (module Option_of (Pair_of (File_path.Part) (File_path.Relative)))
        top_dir_and_all_but_top_dir
        (Option.both (top_dir path) (all_but_top_dir path))
        ~message:
          "[top_dir_and_all_but_top_dir] and [top_dir]/[all_but_top_dir] are inconsistent");
  [%expect
    {|
    (. -> ())
    (.. -> ())
    (singleton -> ())
    (./file -> ((. file)))
    (dir/. -> ((dir .)))
    (../.. -> ((.. ..)))
    (a/b -> ((a b)))
    (a/b/c -> ((a b/c)))
    (a/b/c/d -> ((a b/c/d)))
    (long/chain/of/names/ending/in/this -> ((long chain/of/names/ending/in/this)))
    |}]
;;

let append_to_basename_exn = (File_path.Relative.append_to_basename_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    append_to_basename_exn
    (append_to_basename_exn [@alloc stack])
    (module Fn2_local_exn (File_path.Relative) (String) (File_path.Relative))
    ~examples:Examples.Relative.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (Or_error.ok append_to_basename_exn)
        (if String.mem string '/' || String.mem string '\000'
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
      ("File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix invalid/slash)))))
    ((a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.Relative.append_to_basename_exn: suffix contains invalid characters"
       ((path a/b/c) (suffix "invalid\000null")))))
    ((long/chain/of/names/ending/in -this)
     ->
     (Ok long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = (File_path.Relative.append_part [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append_part
    (append_part [@alloc stack])
    (module Fn2_local (File_path.Relative) (File_path.Part) (File_path.Relative))
    ~examples:Examples.Relative.for_append_part
    ~correctness:(fun (relative, part) append_part ->
      require_equal
        (module Option_of (File_path.Relative))
        (dirname append_part)
        (Some relative)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module File_path.Part)
        (basename append_part)
        part
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
    |}]
;;

let prepend_part = (File_path.Relative.prepend_part [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    prepend_part
    (prepend_part [@alloc stack])
    (module Fn2_local (File_path.Part) (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_prepend_part
    ~correctness:(fun (part, relative) prepend_part ->
      require_equal
        (module Option_of (File_path.Relative))
        (all_but_top_dir prepend_part)
        (Some relative)
        ~message:"[prepend_part] and [all_but_top_dir] are inconsistent";
      require_equal
        (module Option_of (File_path.Part))
        (top_dir prepend_part)
        (Some part)
        ~message:"[prepend_part] and [top_dir] are inconsistent");
  [%expect
    {|
    ((. file) -> ./file)
    ((dir .) -> dir/.)
    ((.. ..) -> ../..)
    ((a b) -> a/b)
    ((a b/c) -> a/b/c)
    ((a b/c/d) -> a/b/c/d)
    ((long chain/of/names/ending/in/this) -> long/chain/of/names/ending/in/this)
    |}]
;;

let is_prefix = File_path.Relative.is_prefix

let%expect_test _ =
  test_predicate
    is_prefix
    (module Fn_labelled (With_prefix_local (File_path.Relative)) (Bool))
    ~examples:Examples.Relative.for_chop_prefix
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
       ((path long/chain/of/names/ending/in/this) (prefix long/chain/of/names))))
     (failure
      (((path ..) (prefix .))
       ((path ./b) (prefix ./a))
       ((path c/d) (prefix a/b))
       ((path a) (prefix a/b/c))
       ((path a/b) (prefix a/b/c)))))
    |}]
;;

let chop_prefix = (File_path.Relative.chop_prefix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_prefix
    (chop_prefix [@alloc stack])
    (module Fn_labelled
              (With_prefix (File_path.Relative)) (Option_of (File_path.Relative)))
    ~examples:Examples.Relative.for_chop_prefix
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
    |}]
;;

let chop_prefix_exn = (File_path.Relative.chop_prefix_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_exn
    (chop_prefix_exn [@alloc stack])
    (module Fn_labelled_exn (With_prefix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
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
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix" ((path ..) (prefix .)))))
    (((path ./b) (prefix ./a))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path ./b) (prefix ./a)))))
    (((path c/d) (prefix a/b))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path c/d) (prefix a/b)))))
    (((path a) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path a) (prefix a/b/c)))))
    (((path a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_exn: not a prefix"
       ((path a/b) (prefix a/b/c)))))
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
    |}]
;;

let chop_prefix_or_error = (File_path.Relative.chop_prefix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_or_error
    (chop_prefix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_prefix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
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
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path ..) (prefix .)))))
    (((path ./b) (prefix ./a))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path ./b) (prefix ./a)))))
    (((path c/d) (prefix a/b))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path c/d) (prefix a/b)))))
    (((path a) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path a) (prefix a/b/c)))))
    (((path a/b) (prefix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_prefix_or_error: not a prefix"
       ((path a/b) (prefix a/b/c)))))
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
    |}]
;;

let chop_prefix_if_exists = (File_path.Relative.chop_prefix_if_exists [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_if_exists
    (chop_prefix_if_exists [@alloc stack])
    (module Fn_labelled (With_prefix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_if_exists ->
      require_equal
        (module File_path.Relative)
        (chop_prefix path ~prefix |> Option.value ~default:path)
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
    |}]
;;

let is_suffix = File_path.Relative.is_suffix

let%expect_test _ =
  test_predicate
    is_suffix
    (module Fn_labelled (With_suffix_local (File_path.Relative)) (Bool))
    ~examples:Examples.Relative.for_chop_suffix
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
       ((path long/chain/of/names/ending/in/this) (suffix ending/in/this))))
     (failure
      (((path ..) (suffix .))
       ((path b/.) (suffix a/.))
       ((path c/d) (suffix a/b))
       ((path c) (suffix a/b/c))
       ((path b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = (File_path.Relative.chop_suffix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_suffix
    (chop_suffix [@alloc stack])
    (module Fn_labelled
              (With_suffix (File_path.Relative)) (Option_of (File_path.Relative)))
    ~examples:Examples.Relative.for_chop_suffix
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
    |}]
;;

let chop_suffix_exn = (File_path.Relative.chop_suffix_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_exn
    (chop_suffix_exn [@alloc stack])
    (module Fn_labelled_exn (With_suffix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix" ((path ..) (suffix .)))))
    (((path b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path b/.) (suffix a/.)))))
    (((path c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path c/d) (suffix a/b)))))
    (((path c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path c) (suffix a/b/c)))))
    (((path b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_exn: not a suffix"
       ((path b/c) (suffix a/b/c)))))
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
    |}]
;;

let chop_suffix_or_error = (File_path.Relative.chop_suffix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_or_error
    (chop_suffix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_suffix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path ..) (suffix .))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path ..) (suffix .)))))
    (((path b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path b/.) (suffix a/.)))))
    (((path c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path c/d) (suffix a/b)))))
    (((path c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path c) (suffix a/b/c)))))
    (((path b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Relative.chop_suffix_or_error: not a suffix"
       ((path b/c) (suffix a/b/c)))))
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
    |}]
;;

let chop_suffix_if_exists = (File_path.Relative.chop_suffix_if_exists [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_if_exists
    (chop_suffix_if_exists [@alloc stack])
    (module Fn_labelled (With_suffix (File_path.Relative)) (File_path.Relative))
    ~examples:Examples.Relative.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path.Relative)
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
    |}]
;;

let append = (File_path.Relative.append [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append
    (append [@alloc stack])
    (module Fn2_local (File_path.Relative) (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Option_of (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Option_of (File_path.Relative))
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
    |}]
;;

let to_parts = (File_path.Relative.to_parts [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_parts
    (to_parts [@alloc stack])
    (module Fn (File_path.Relative) (List_of (File_path.Part)))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts relative)
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
    |}]
;;

let to_parts_nonempty = (File_path.Relative.to_parts_nonempty [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    to_parts_nonempty
    (to_parts_nonempty [@alloc stack])
    (module Fn (File_path.Relative) (Nonempty_list_of (File_path.Part)))
    ~examples:Examples.Relative.for_conversion
    ~correctness:(fun relative to_parts_nonempty ->
      require_equal
        (module List_of (File_path.Part))
        (Nonempty_list.to_list to_parts_nonempty)
        (to_parts relative)
        ~message:"[to_parts_nonempty] and [to_parts] are inconsistent");
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
    |}]
;;

let of_parts = (File_path.Relative.of_parts [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    of_parts
    (of_parts [@alloc stack])
    (module Fn (List_of (File_path.Part)) (Option_of (File_path.Relative)))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts ->
      require_equal
        (module List_of (File_path.Part))
        (Option.value_map of_parts ~f:to_parts ~default:[])
        parts
        ~message:"[of_parts] and [to_parts] are inconsistent");
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

let of_parts_exn = (File_path.Relative.of_parts_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_exn
    (of_parts_exn [@alloc stack])
    (module Fn_exn (List_of (File_path.Part)) (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (of_parts parts)
        (Or_error.ok of_parts_exn)
        ~message:"[of_parts_exn] and [of_parts] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.Relative.of_parts_exn: empty list"))
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

let of_parts_or_error = (File_path.Relative.of_parts_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_or_error
    (of_parts_or_error [@alloc stack])
    (module Fn_or_error (List_of (File_path.Part)) (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (of_parts parts)
        (Or_error.ok of_parts_or_error)
        ~message:"[of_parts_or_error] and [of_parts] are inconsistent");
  [%expect
    {|
    (() -> (Error "File_path.Relative.of_parts_or_error: empty list"))
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

let of_parts_defaulting_to_dot =
  (File_path.Relative.of_parts_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_defaulting_to_dot
    (of_parts_defaulting_to_dot [@alloc stack])
    (module Fn (List_of (File_path.Part)) (File_path.Relative))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts_defaulting_to_dot ->
      require_equal
        (module File_path.Relative)
        (Option.value (of_parts parts) ~default:dot)
        of_parts_defaulting_to_dot
        ~message:"[of_parts_defaulting_to_dot] and [of_parts] are inconsistent");
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

let of_parts_nonempty = (File_path.Relative.of_parts_nonempty [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    of_parts_nonempty
    (of_parts_nonempty [@alloc stack])
    (module Fn (Nonempty_list_of (File_path.Part)) (File_path.Relative))
    ~examples:
      (List.filter_map Examples.Part.lists_for_conversion ~f:Nonempty_list.of_list)
    ~correctness:(fun parts of_parts_nonempty ->
      require_equal
        (module Option_of (File_path.Relative))
        (of_parts (Nonempty_list.to_list parts))
        (Some of_parts_nonempty)
        ~message:"[of_parts_nonempty] and [of_parts] are inconsistent");
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

let simplify_dot = (File_path.Relative.simplify_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    simplify_dot
    (simplify_dot [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module File_path.Relative)
        simplified
        (to_parts original
         |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot)
         |> of_parts_defaulting_to_dot)
        ~message:"[simplify_dot] and is not equivalent to filtering out [.]";
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
    |}]
;;

let simplify_dot_and_dot_dot_naively =
  (File_path.Relative.simplify_dot_and_dot_dot_naively [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    simplify_dot_and_dot_dot_naively
    (simplify_dot_and_dot_dot_naively [@alloc stack])
    (module Fn (File_path.Relative) (File_path.Relative))
    ~examples:Examples.Relative.for_simplify
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
        (module File_path.Relative)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path.Relative)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path.Relative)
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
    |}]
;;]
