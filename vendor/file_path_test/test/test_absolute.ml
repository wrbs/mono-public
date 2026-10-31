(* See comment in [test_path.ml]. *)

open! Core
open Expect_test_helpers_core
open Helpers

let root = File_path.Absolute.root

let%expect_test _ =
  test_constants (module File_path.Absolute) [ root ];
  [%expect {| / |}]
;;

open struct
  module Common = Test_common.Make (struct
      module Type = File_path.Types.Absolute
      module Path = File_path.Absolute
      module Examples = Examples.Absolute
      module Tested = Test_absolute_completion
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
     "/\255\001")

    Testing: of_string
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
    (~ // /)
    (~ //. /.)
    (~ /./ /.)
    (~ /.//. /./.)
    (~ /.//.// /./.)
    (~ /bin/exe/ /bin/exe)
    (~ /bin//exe//file /bin/exe/file)
    (~ /bin//exe//file/ /bin/exe/file)
    (! ("File_path.Absolute.of_string: invalid string" ""))
    (! ("File_path.Absolute.of_string: invalid string" "/invalid/\000/null"))
    (! ("File_path.Absolute.of_string: invalid string" invalid/relative))

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
      /filename.txt))
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
      (/filename.txt 12)))
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
      /filename.txt))
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
      (/filename.txt 12)))

    Testing: invariant
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
    (! ("File_path.Absolute.invariant: non-canonical representation" //))
    (! ("File_path.Absolute.invariant: non-canonical representation" //.))
    (! ("File_path.Absolute.invariant: non-canonical representation" /./))
    (! ("File_path.Absolute.invariant: non-canonical representation" /.//.))
    (! ("File_path.Absolute.invariant: non-canonical representation" /.//.//))
    (! ("File_path.Absolute.invariant: non-canonical representation" /bin/exe/))
    (!
     ("File_path.Absolute.invariant: non-canonical representation"
      /bin//exe//file))
    (!
     ("File_path.Absolute.invariant: non-canonical representation"
      /bin//exe//file/))
    (! ("File_path.Absolute.invariant: invalid string" ""))
    (! ("File_path.Absolute.invariant: invalid string" "/invalid/\000/null"))
    (! ("File_path.Absolute.invariant: invalid string" invalid/relative))
    |}]
;;

[%%template
let basename = (File_path.Absolute.basename [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    basename
    (basename [@alloc stack])
    (module Fn (File_path.Absolute) (Option_of (File_path.Part)))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename ->
      require_equal
        (module Bool)
        (Option.is_none basename)
        (equal absolute root)
        ~message:"[basename] is inconsistent with [equal root]");
  [%expect
    {|
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

let basename_exn = (File_path.Absolute.basename_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    basename_exn
    (basename_exn [@alloc stack])
    (module Fn_exn (File_path.Absolute) (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_exn ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok basename_exn)
        (basename absolute)
        ~message:"[basename_exn] and [basename] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.basename_exn: root path"))
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

let basename_or_error = (File_path.Absolute.basename_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    basename_or_error
    (basename_or_error [@alloc stack])
    (module Fn_or_error (File_path.Absolute) (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_or_error ->
      require_equal
        (module Option_of (File_path.Part))
        (Or_error.ok basename_or_error)
        (basename absolute)
        ~message:"[basename_or_error] and [basename] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.basename_or_error: root path"))
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

let basename_defaulting_to_dot =
  (File_path.Absolute.basename_defaulting_to_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    basename_defaulting_to_dot
    (basename_defaulting_to_dot [@alloc stack])
    (module Fn (File_path.Absolute) (File_path.Part))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute basename_defaulting_to_dot ->
      require_equal
        (module File_path.Part)
        basename_defaulting_to_dot
        (Option.value (basename absolute) ~default:File_path.Part.dot)
        ~message:"[basename_defaulting_to_dot] and [basename] are inconsistent");
  [%expect
    {|
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

let dirname = (File_path.Absolute.dirname [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname
    (dirname [@alloc stack])
    (module Fn (File_path.Absolute) (Option_of (File_path.Absolute)))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname ->
      require_equal
        (module Bool)
        (Option.is_none dirname)
        (Option.is_none (basename absolute))
        ~message:"[dirname] and [basename] are inconsistent");
  [%expect
    {|
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

let dirname_exn = (File_path.Absolute.dirname_exn [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    dirname_exn
    (dirname_exn [@alloc stack])
    (module Fn_exn (File_path.Absolute) (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_exn ->
      require_equal
        (module Option_of (File_path.Absolute))
        (Or_error.ok dirname_exn)
        (dirname absolute)
        ~message:"[dirname_exn] and [dirname] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.dirname_exn: root path"))
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

let dirname_or_error = (File_path.Absolute.dirname_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_or_error
    (dirname_or_error [@alloc stack])
    (module Fn_or_error (File_path.Absolute) (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_or_error ->
      require_equal
        (module Option_of (File_path.Absolute))
        (Or_error.ok dirname_or_error)
        (dirname absolute)
        ~message:"[dirname_or_error] and [dirname] are inconsistent");
  [%expect
    {|
    (/ -> (Error "File_path.Absolute.dirname_or_error: root path"))
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

let dirname_defaulting_to_root =
  (File_path.Absolute.dirname_defaulting_to_root [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_defaulting_to_root
    (dirname_defaulting_to_root [@alloc stack])
    (module Fn (File_path.Absolute) (File_path.Absolute))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun absolute dirname_defaulting_to_root ->
      require_equal
        (module File_path.Absolute)
        dirname_defaulting_to_root
        (Option.value (dirname absolute) ~default:root)
        ~message:"[dirname_defaulting_to_root] and [dirname] are inconsistent");
  [%expect
    {|
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

let dirname_and_basename = (File_path.Absolute.dirname_and_basename [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    dirname_and_basename
    (dirname_and_basename [@alloc stack])
    (module Fn
              (File_path.Absolute)
              (Option_of (Pair_of (File_path.Absolute) (File_path.Part))))
    ~examples:Examples.Absolute.for_basename_and_dirname
    ~correctness:(fun path dirname_and_basename ->
      require_equal
        (module Option_of (Pair_of (File_path.Absolute) (File_path.Part)))
        dirname_and_basename
        (Option.both (dirname path) (basename path))
        ~message:"[dirname_and_basename] and [dirname]/[basename] are inconsistent");
  [%expect
    {|
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

let append_to_basename_exn = (File_path.Absolute.append_to_basename_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    append_to_basename_exn
    (append_to_basename_exn [@alloc stack])
    (module Fn2_local_exn (File_path.Absolute) (String) (File_path.Absolute))
    ~examples:Examples.Absolute.for_append_to_basename
    ~correctness:(fun (path, string) append_to_basename_exn ->
      require_equal
        (module Option_of (File_path.Absolute))
        (Or_error.ok append_to_basename_exn)
        (if equal path root || String.mem string '/' || String.mem string '\000'
         then None
         else Some (of_string (to_string path ^ string))));
  [%expect
    {|
    ((/ "")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix "")))))
    ((/ x)
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix x)))))
    ((/ invalid/slash)
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
       ((path /) (suffix invalid/slash)))))
    ((/ "invalid\000null")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: root path has no basename"
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
      ("File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix invalid/slash)))))
    ((/a/b/c "invalid\000null")
     ->
     (Error
      ("File_path.Absolute.append_to_basename_exn: suffix contains invalid characters"
       ((path /a/b/c) (suffix "invalid\000null")))))
    ((/long/chain/of/names/ending/in -this)
     ->
     (Ok /long/chain/of/names/ending/in-this))
    |}]
;;

let append_part = (File_path.Absolute.append_part [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append_part
    (append_part [@alloc stack])
    (module Fn2_local (File_path.Absolute) (File_path.Part) (File_path.Absolute))
    ~examples:Examples.Absolute.for_append_part
    ~correctness:(fun (absolute, part) append_part ->
      require_equal
        (module Option_of (File_path.Absolute))
        (dirname append_part)
        (Some absolute)
        ~message:"[append_part] and [dirname] are inconsistent";
      require_equal
        (module Option_of (File_path.Part))
        (basename append_part)
        (Some part)
        ~message:"[append_part] and [dirname] are inconsistent");
  [%expect
    {|
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

let is_prefix = File_path.Absolute.is_prefix

let%expect_test _ =
  test_predicate
    is_prefix
    (module Fn_labelled (With_prefix_local (File_path.Absolute)) (Bool))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((path /) (prefix /))
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
      (((path /..) (prefix /.))
       ((path /./b) (prefix /./a))
       ((path /c/d) (prefix /a/b))
       ((path /a) (prefix /a/b/c))
       ((path /a/b) (prefix /a/b/c)))))
    |}]
;;

let chop_prefix = (File_path.Absolute.chop_prefix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_prefix
    (chop_prefix [@alloc stack])
    (module Fn_labelled
              (With_prefix (File_path.Absolute)) (Option_of (File_path.Relative)))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix ->
      require_equal
        (module Bool)
        (is_prefix path ~prefix)
        (Option.is_some chop_prefix)
        ~message:"[chop_prefix] and [is_prefix] are inconsistent");
  [%expect
    {|
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
    |}]
;;

let chop_prefix_exn = (File_path.Absolute.chop_prefix_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_exn
    (chop_prefix_exn [@alloc stack])
    (module Fn_labelled_exn (With_prefix (File_path.Absolute)) (File_path.Relative))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_exn ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_prefix path ~prefix)
        (Or_error.ok chop_prefix_exn)
        ~message:"[chop_prefix_exn] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path /) (prefix /)) -> (Ok .))
    (((path /..) (prefix /.))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /..) (prefix /.)))))
    (((path /./b) (prefix /./a))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /./b) (prefix /./a)))))
    (((path /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /c/d) (prefix /a/b)))))
    (((path /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
       ((path /a) (prefix /a/b/c)))))
    (((path /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_exn: not a prefix"
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
    |}]
;;

let chop_prefix_or_error = (File_path.Absolute.chop_prefix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_prefix_or_error
    (chop_prefix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_prefix (File_path.Absolute)) (File_path.Relative))
    ~examples:Examples.Absolute.for_chop_prefix
    ~correctness:(fun { path; prefix } chop_prefix_or_error ->
      require_equal
        (module Option_of (File_path.Relative))
        (chop_prefix path ~prefix)
        (Or_error.ok chop_prefix_or_error)
        ~message:"[chop_prefix_or_error] and [chop_prefix] are inconsistent");
  [%expect
    {|
    (((path /) (prefix /)) -> (Ok .))
    (((path /..) (prefix /.))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /..) (prefix /.)))))
    (((path /./b) (prefix /./a))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /./b) (prefix /./a)))))
    (((path /c/d) (prefix /a/b))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /c/d) (prefix /a/b)))))
    (((path /a) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
       ((path /a) (prefix /a/b/c)))))
    (((path /a/b) (prefix /a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_prefix_or_error: not a prefix"
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
    |}]
;;

let is_suffix = File_path.Absolute.is_suffix

let%expect_test _ =
  test_predicate
    is_suffix
    (module Fn_labelled (With_suffix_local (File_path.Absolute)) (Bool))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
    ((success
      (((path /.) (suffix .))
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
      (((path /) (suffix .))
       ((path /..) (suffix .))
       ((path /b/.) (suffix a/.))
       ((path /c/d) (suffix a/b))
       ((path /c) (suffix a/b/c))
       ((path /b/c) (suffix a/b/c)))))
    |}]
;;

let chop_suffix = (File_path.Absolute.chop_suffix [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    chop_suffix
    (chop_suffix [@alloc stack])
    (module Fn_labelled
              (With_suffix (File_path.Absolute)) (Option_of (File_path.Absolute)))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix ->
      require_equal
        (module Bool)
        (is_suffix path ~suffix)
        (Option.is_some chop_suffix)
        ~message:"[chop_suffix] and [is_suffix] are inconsistent");
  [%expect
    {|
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

let chop_suffix_exn = (File_path.Absolute.chop_suffix_exn [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_exn
    (chop_suffix_exn [@alloc stack])
    (module Fn_labelled_exn (With_suffix (File_path.Absolute)) (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_exn ->
      require_equal
        (module Option_of (File_path.Absolute))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_exn)
        ~message:"[chop_suffix_exn] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path /) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix" ((path /) (suffix .)))))
    (((path /..) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix" ((path /..) (suffix .)))))
    (((path /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /b/.) (suffix a/.)))))
    (((path /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /c/d) (suffix a/b)))))
    (((path /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
       ((path /c) (suffix a/b/c)))))
    (((path /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_exn: not a suffix"
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

let chop_suffix_or_error = (File_path.Absolute.chop_suffix_or_error [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_or_error
    (chop_suffix_or_error [@alloc stack])
    (module Fn_labelled_or_error (With_suffix (File_path.Absolute)) (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_or_error ->
      require_equal
        (module Option_of (File_path.Absolute))
        (chop_suffix path ~suffix)
        (Or_error.ok chop_suffix_or_error)
        ~message:"[chop_suffix_or_error] and [chop_suffix] are inconsistent");
  [%expect
    {|
    (((path /) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /) (suffix .)))))
    (((path /..) (suffix .))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /..) (suffix .)))))
    (((path /b/.) (suffix a/.))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /b/.) (suffix a/.)))))
    (((path /c/d) (suffix a/b))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /c/d) (suffix a/b)))))
    (((path /c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
       ((path /c) (suffix a/b/c)))))
    (((path /b/c) (suffix a/b/c))
     ->
     (Error
      ("File_path.Absolute.chop_suffix_or_error: not a suffix"
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

let chop_suffix_if_exists = (File_path.Absolute.chop_suffix_if_exists [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    chop_suffix_if_exists
    (chop_suffix_if_exists [@alloc stack])
    (module Fn_labelled (With_suffix (File_path.Absolute)) (File_path.Absolute))
    ~examples:Examples.Absolute.for_chop_suffix
    ~correctness:(fun { path; suffix } chop_suffix_if_exists ->
      require_equal
        (module File_path.Absolute)
        (chop_suffix path ~suffix |> Option.value ~default:path)
        chop_suffix_if_exists
        ~message:"[chop_suffix_if_exists] and [chop_suffix] are inconsistent");
  [%expect
    {|
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

let append = (File_path.Absolute.append [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    append
    (append [@alloc stack])
    (module Fn2_local (File_path.Absolute) (File_path.Relative) (File_path.Absolute))
    ~examples:Examples.Absolute.for_append
    ~correctness:(fun (prefix, suffix) append ->
      require_equal
        (module Option_of (File_path.Relative))
        (Some suffix)
        (chop_prefix append ~prefix)
        ~message:"[append] and [chop_prefix] are inconsistent";
      require_equal
        (module Option_of (File_path.Absolute))
        (Some prefix)
        (chop_suffix append ~suffix)
        ~message:"[append] and [chop_suffix] are inconsistent");
  [%expect
    {|
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

let number_of_parts = File_path.Absolute.number_of_parts

let%expect_test _ =
  test_immediate
    number_of_parts
    (module Fn_local (File_path.Absolute) (Int))
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun _ _ -> (* tested for correctness below *) ());
  [%expect
    {|
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

let to_parts = (File_path.Absolute.to_parts [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    to_parts
    (to_parts [@alloc stack])
    (module Fn (File_path.Absolute) (List_of (File_path.Part)))
    ~examples:Examples.Absolute.for_conversion
    ~correctness:(fun absolute to_parts ->
      require_equal
        (module Int)
        (List.length to_parts)
        (number_of_parts absolute)
        ~message:"[to_parts] and [number_of_parts] are inconsistent");
  [%expect
    {|
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

let of_parts = (File_path.Absolute.of_parts [@alloc a]) [@@alloc a = (heap, stack)]

let%expect_test _ =
  test_function
    of_parts
    (of_parts [@alloc stack])
    (module Fn_local (List_of (File_path.Part)) (File_path.Absolute))
    ~examples:Examples.Part.lists_for_conversion
    ~correctness:(fun parts of_parts ->
      require_equal
        (module List_of (File_path.Part))
        (to_parts of_parts)
        parts
        ~message:"[of_parts] and [to_parts] are inconsistent");
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

let simplify_dot = (File_path.Absolute.simplify_dot [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    simplify_dot
    (simplify_dot [@alloc stack])
    (module Fn (File_path.Absolute) (File_path.Absolute))
    ~examples:Examples.Absolute.for_simplify
    ~correctness:(fun original simplified ->
      require_equal
        (module List_of (File_path.Part))
        (to_parts simplified)
        (to_parts original |> List.filter ~f:(File_path.Part.( <> ) File_path.Part.dot))
        ~message:"[simplify_dot] is not equivalent to filtering out [.]";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot original) : t)));
  [%expect
    {|
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
  (File_path.Absolute.simplify_dot_and_dot_dot_naively [@alloc a])
[@@alloc a = (heap, stack)]
;;

let%expect_test _ =
  test_function
    simplify_dot_and_dot_dot_naively
    (simplify_dot_and_dot_dot_naively [@alloc stack])
    (module Fn (File_path.Absolute) (File_path.Absolute))
    ~examples:Examples.Absolute.for_simplify
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
        (module File_path.Absolute)
        simplified
        (simplify_dot_and_dot_dot_naively (simplify_dot original))
        ~message:"[simplify_dot_and_dot_dot_naively] does not ignore [.] parts";
      require_equal
        (module File_path.Absolute)
        simplified
        (simplify_dot simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] does not simplify all [.] parts";
      require_equal
        (module File_path.Absolute)
        simplified
        (simplify_dot_and_dot_dot_naively simplified)
        ~message:"[simplify_dot_and_dot_dot_naively] is not idempotent";
      if equal original simplified
      then
        require_no_allocation (fun () ->
          ignore (Sys.opaque_identity (simplify_dot_and_dot_dot_naively original) : t)));
  [%expect
    {|
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
