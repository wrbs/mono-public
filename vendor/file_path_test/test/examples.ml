(* The easiest way to see what examples this library actually generates is to check expect
   test output in [test_*.ml] files.

   This file does not guarantee comprehensive coverage, which is part of why we also use
   quickcheck, although even the combination is not a hard-and-fast guarantee. The
   purposes of these examples are (1) allowing human inspection of expect tests and (2)
   giving some reasonable confidence that we have covered many important cases. If we
   discover more important cases in the future, we should add them here. *)

open! Core
include Examples_intf.Definitions

module With_if_under = struct
  include With_if_under

  let create path ~if_under = { path; if_under }
  let map { path; if_under } ~f = { path = f path; if_under }

  let invariant a_invariant { path; if_under } =
    a_invariant path;
    File_path.Absolute.invariant if_under
  ;;
end

module With_prefix = struct
  include With_prefix

  let create path ~prefix = { path; prefix }
  let map { path; prefix } ~f = { path = f path; prefix = f prefix }

  let invariant a_invariant { path; prefix } =
    a_invariant path;
    a_invariant prefix
  ;;
end

module With_suffix = struct
  include With_suffix

  let create path ~suffix = { path; suffix }
  let map { path; suffix } ~f = { path = f path; suffix }

  let invariant a_invariant { path; suffix } =
    a_invariant path;
    File_path.Relative.invariant suffix
  ;;
end

module With_under = struct
  include With_under

  let create path ~under = { path; under }
  let map { path; under } ~f = { path = f path; under }

  let invariant a_invariant { path; under } =
    a_invariant path;
    File_path.Absolute.invariant under
  ;;
end

let pair_map ~f1 ~f2 (x1, x2) = f1 x1, f2 x2

(* prompts us to think about all three cases *)
let list_for_of_string ~canonical ~non_canonical ~invalid =
  List.concat [ canonical; non_canonical; invalid ]
;;

module Part = struct
  let of_string = File_path.Part.of_string

  let canonical =
    [ "."
    ; ".."
    ; "filename.txt"
    ; "bin"
    ; ".hidden"
    ; "This is a sentence; it has punctuation, capitalization, and spaces!"
    ; "\001\255"
    ]
  ;;

  let non_canonical = []
  let invalid = [ ""; "invalid/slash"; "invalid\000null" ]
  let strings_for_of_string = list_for_of_string ~canonical ~non_canonical ~invalid
  let strings_for_conversion = canonical

  let strings_for_compare =
    [ "."
    ; ".."
    ; "filename.txt"
    ; "bin"
    ; "bin.exe"
    ; "binary"
    ; ".hidden"
    ; "-dot-is-not-always-first"
    ; "This is a sentence; it has punctuation, capitalization, and spaces!"
    ; "\001\255"
    ; "\255\001"
    ]
  ;;

  let strings_for_append_to_basename =
    [ ".", "x"
    ; "..", "y"
    ; "a", ".b"
    ; "b", "invalid/slash"
    ; "c", "invalid\000null"
    ; "long-hyphenated-name-ending-in", "-this"
    ]
  ;;

  let string_lists_for_conversion =
    let len0 = [ [] ] in
    let len1 = List.map strings_for_conversion ~f:List.return in
    let len2 =
      List.concat_map strings_for_conversion ~f:(fun part ->
        [ [ part; "." ]; [ ".."; part ] ])
      @ [ [ ".hidden"; "bin.exe" ] ]
    in
    let len3 = [ [ ".hidden"; "bin"; "exe.file" ] ] in
    List.concat [ len0; len1; len2; len3 ]
  ;;

  let for_conversion = strings_for_conversion |> List.map ~f:of_string
  let for_compare = strings_for_compare |> List.map ~f:of_string

  let for_append_to_basename =
    strings_for_append_to_basename |> List.map ~f:(pair_map ~f1:of_string ~f2:Fn.id)
  ;;

  let lists_for_conversion =
    string_lists_for_conversion |> List.map ~f:(List.map ~f:of_string)
  ;;
end

module Relative = struct
  let of_string = File_path.Relative.of_string
  let canonical = Part.canonical @ [ "./."; "../.."; "././."; "bin/exe"; "bin/exe/file" ]

  let non_canonical =
    [ "./"; ".//."; ".//.//"; "bin/exe/"; "bin//exe//file"; "bin//exe//file/" ]
  ;;

  let invalid = [ ""; "invalid/\000/null"; "/invalid/absolute" ]
  let strings_for_of_string = list_for_of_string ~canonical ~non_canonical ~invalid
  let strings_for_conversion = canonical

  let strings_for_compare =
    Part.strings_for_compare
    @ [ "./."
      ; "./.."
      ; "../."
      ; "../.."
      ; "././."
      ; "bin/exe"
      ; "bin/exe/file"
      ; "bin/exe.file"
      ]
  ;;

  let strings_for_append_to_basename =
    [ ".", "x"
    ; "..", "y"
    ; "a", ".b"
    ; "a/b", ".c"
    ; "a/b/c", ".d"
    ; "a/b/c", ""
    ; "a/b/c", "invalid/slash"
    ; "a/b/c", "invalid\000null"
    ; "long/chain/of/names/ending/in", "-this"
    ]
  ;;

  let strings_for_append_part =
    [ ".", "file"
    ; "dir", "."
    ; "..", ".."
    ; "a", "b"
    ; "a/b", "c"
    ; "a/b/c", "d"
    ; "long/chain/of/names/ending/in", "this"
    ]
  ;;

  let strings_for_basename_and_dirname =
    [ "."; ".."; "singleton" ]
    @ List.map strings_for_append_part ~f:(fun (basename, dirname) ->
      basename ^ "/" ^ dirname)
  ;;

  let strings_for_prepend_part =
    [ ".", "file"
    ; "dir", "."
    ; "..", ".."
    ; "a", "b"
    ; "a", "b/c"
    ; "a", "b/c/d"
    ; "long", "chain/of/names/ending/in/this"
    ]
  ;;

  let strings_for_top_dir =
    [ "."; ".."; "singleton" ]
    @ List.map strings_for_prepend_part ~f:(fun (part, relative) -> part ^ "/" ^ relative)
  ;;

  let strings_for_append =
    [ ".", "file"
    ; "dir", "."
    ; "..", ".."
    ; "a", "b/c/d"
    ; "a/b", "c/d"
    ; "a/b/c", "d"
    ; "long/chain/of/names", "ending/in/this"
    ]
  ;;

  let strings_for_simplify =
    Part.strings_for_conversion
    @ [ "a/b"
      ; "a/b/."
      ; "a/./b"
      ; "./a/b"
      ; "./a/./b/."
      ; "a/b/./."
      ; "a/././b"
      ; "././a/b"
      ; "././a/././b/./."
      ; "a/b/.."
      ; "a/../b"
      ; "../a/b"
      ; "../a/../b/.."
      ; "a/b/../.."
      ; "a/../../b"
      ; "../../a/b"
      ; "../../a/../../b/../.."
      ; "a/b/./.."
      ; "a/./../b"
      ; "./../a/b"
      ; "./../a/./../b/./.."
      ; "a/b/../."
      ; "a/.././b"
      ; ".././a/b"
      ; ".././a/.././b/../."
      ]
  ;;

  let strings_for_chop_prefix =
    let open With_prefix in
    [ { prefix = "."; path = ".." }
    ; { prefix = "./a"; path = "./b" }
    ; { prefix = "a/b"; path = "c/d" }
    ; { prefix = "a/b/c"; path = "a" }
    ; { prefix = "a/b/c"; path = "a/b" }
    ]
    @ List.map [ "."; ".."; "a/b/c" ] ~f:(fun string ->
      { prefix = string; path = string })
    @ List.map strings_for_append ~f:(fun (prefix, suffix) ->
      { prefix; path = prefix ^ "/" ^ suffix })
  ;;

  let strings_for_chop_suffix =
    let open With_suffix in
    [ { suffix = File_path.Relative.of_string "."; path = ".." }
    ; { suffix = File_path.Relative.of_string "a/."; path = "b/." }
    ; { suffix = File_path.Relative.of_string "a/b"; path = "c/d" }
    ; { suffix = File_path.Relative.of_string "a/b/c"; path = "c" }
    ; { suffix = File_path.Relative.of_string "a/b/c"; path = "b/c" }
    ]
    @ List.map [ "."; ".."; "a/b/c" ] ~f:(fun string ->
      { suffix = File_path.Relative.of_string string; path = string })
    @ List.map strings_for_append ~f:(fun (prefix, suffix) ->
      { suffix = File_path.Relative.of_string suffix; path = prefix ^ "/" ^ suffix })
  ;;

  let for_conversion = strings_for_conversion |> List.map ~f:of_string
  let for_compare = strings_for_compare |> List.map ~f:of_string
  let for_basename_and_dirname = strings_for_basename_and_dirname |> List.map ~f:of_string
  let for_top_dir = strings_for_top_dir |> List.map ~f:of_string

  let for_append_to_basename =
    strings_for_append_to_basename |> List.map ~f:(pair_map ~f1:of_string ~f2:Fn.id)
  ;;

  let for_append_part =
    strings_for_append_part |> List.map ~f:(pair_map ~f1:of_string ~f2:Part.of_string)
  ;;

  let for_prepend_part =
    strings_for_prepend_part |> List.map ~f:(pair_map ~f1:Part.of_string ~f2:of_string)
  ;;

  let for_chop_prefix =
    strings_for_chop_prefix |> List.map ~f:(With_prefix.map ~f:of_string)
  ;;

  let for_chop_suffix =
    strings_for_chop_suffix |> List.map ~f:(With_suffix.map ~f:of_string)
  ;;

  let for_append =
    strings_for_append |> List.map ~f:(pair_map ~f1:of_string ~f2:of_string)
  ;;

  let for_simplify = strings_for_simplify |> List.map ~f:of_string
end

module Absolute = struct
  let of_string = File_path.Absolute.of_string
  let make_absolute string = "/" ^ string
  let canonical = [ "/" ] @ List.map Relative.canonical ~f:make_absolute
  let non_canonical = [ "//"; "//." ] @ List.map Relative.non_canonical ~f:make_absolute
  let invalid = [ ""; "/invalid/\000/null"; "invalid/relative" ]
  let strings_for_of_string = list_for_of_string ~canonical ~non_canonical ~invalid

  let strings_for_conversion =
    [ "/" ] @ List.map Relative.strings_for_conversion ~f:make_absolute
  ;;

  let strings_for_compare =
    [ "/" ] @ List.map Relative.strings_for_compare ~f:make_absolute
  ;;

  let strings_for_basename_and_dirname =
    [ "/" ] @ List.map Relative.strings_for_basename_and_dirname ~f:make_absolute
  ;;

  let strings_for_append_to_basename =
    [ "/", ""; "/", "x"; "/", "invalid/slash"; "/", "invalid\000null" ]
    @ List.map
        Relative.strings_for_append_to_basename
        ~f:(pair_map ~f1:make_absolute ~f2:Fn.id)
  ;;

  let strings_for_append_part =
    [ "/", "."; "/", ".."; "/", "singleton" ]
    @ List.map Relative.strings_for_append_part ~f:(pair_map ~f1:make_absolute ~f2:Fn.id)
  ;;

  let strings_for_append =
    [ "/", "."; "/", ".."; "/", "a/b/c" ]
    @ List.map Relative.strings_for_append ~f:(pair_map ~f1:make_absolute ~f2:Fn.id)
  ;;

  let strings_for_chop_prefix =
    [ With_prefix.create "/" ~prefix:"/" ]
    @ List.map Relative.strings_for_chop_prefix ~f:(With_prefix.map ~f:make_absolute)
  ;;

  let strings_for_chop_suffix =
    [ With_suffix.create "/" ~suffix:File_path.Relative.dot ]
    @ List.map Relative.strings_for_chop_suffix ~f:(fun with_suffix ->
      { with_suffix with path = make_absolute with_suffix.path })
  ;;

  let strings_for_simplify =
    [ "/" ] @ List.map Relative.strings_for_simplify ~f:make_absolute
  ;;

  let for_conversion = strings_for_conversion |> List.map ~f:of_string
  let for_compare = strings_for_compare |> List.map ~f:of_string
  let for_basename_and_dirname = strings_for_basename_and_dirname |> List.map ~f:of_string

  let for_append_to_basename =
    strings_for_append_to_basename |> List.map ~f:(pair_map ~f1:of_string ~f2:Fn.id)
  ;;

  let for_append_part =
    strings_for_append_part |> List.map ~f:(pair_map ~f1:of_string ~f2:Part.of_string)
  ;;

  let for_chop_prefix =
    strings_for_chop_prefix |> List.map ~f:(With_prefix.map ~f:of_string)
  ;;

  let for_chop_suffix =
    strings_for_chop_suffix |> List.map ~f:(With_suffix.map ~f:of_string)
  ;;

  let for_append =
    strings_for_append |> List.map ~f:(pair_map ~f1:of_string ~f2:Relative.of_string)
  ;;

  let for_simplify = strings_for_simplify |> List.map ~f:of_string
end

module Path = struct
  let of_string = File_path.of_string
  let canonical = Relative.canonical @ Absolute.canonical
  let non_canonical = Relative.non_canonical @ Absolute.non_canonical
  let invalid = [ ""; "invalid/\000/null" ]
  let strings_for_of_string = list_for_of_string ~canonical ~non_canonical ~invalid

  let strings_for_conversion =
    Relative.strings_for_conversion @ Absolute.strings_for_conversion
  ;;

  let strings_for_compare = Relative.strings_for_compare @ Absolute.strings_for_compare

  let strings_for_basename_and_dirname =
    Relative.strings_for_basename_and_dirname @ Absolute.strings_for_basename_and_dirname
  ;;

  let strings_for_append_to_basename =
    Relative.strings_for_append_to_basename @ Absolute.strings_for_append_to_basename
  ;;

  let strings_for_append_part =
    Relative.strings_for_append_part @ Absolute.strings_for_append_part
  ;;

  let strings_for_append = Relative.strings_for_append @ Absolute.strings_for_append

  let strings_for_chop_prefix =
    Relative.strings_for_chop_prefix
    @ Absolute.strings_for_chop_prefix
    @ [ { path = "/"; prefix = "." }
      ; { path = "."; prefix = "/" }
      ; { path = "/a/b/c"; prefix = "a/b" }
      ; { path = "a/b/c"; prefix = "/a/b" }
      ]
  ;;

  let strings_for_chop_suffix =
    Relative.strings_for_chop_suffix @ Absolute.strings_for_chop_suffix
  ;;

  let strings_for_make_absolute =
    List.map Absolute.strings_for_append ~f:(fun (prefix, suffix) -> suffix, prefix)
    @ [ "/", "/." ]
    @ List.map Absolute.strings_for_append ~f:(fun (prefix, suffix) ->
      Absolute.make_absolute suffix, prefix)
    |> List.map ~f:(fun (t, under) ->
      With_under.create t ~under:(Absolute.of_string under))
  ;;

  let strings_for_make_relative =
    List.map Absolute.strings_for_append ~f:(fun (prefix, suffix) -> suffix, prefix)
    @ List.map Absolute.strings_for_chop_prefix ~f:(fun { path; prefix } -> path, prefix)
    |> List.map ~f:(fun (path, if_under) ->
      With_if_under.create path ~if_under:(Absolute.of_string if_under))
  ;;

  let strings_for_variant = strings_for_conversion
  let strings_for_simplify = Relative.strings_for_simplify @ Absolute.strings_for_simplify
  let for_conversion = strings_for_conversion |> List.map ~f:of_string
  let for_compare = strings_for_compare |> List.map ~f:of_string
  let for_basename_and_dirname = strings_for_basename_and_dirname |> List.map ~f:of_string

  let for_append_to_basename =
    strings_for_append_to_basename |> List.map ~f:(pair_map ~f1:of_string ~f2:Fn.id)
  ;;

  let for_append_part =
    strings_for_append_part |> List.map ~f:(pair_map ~f1:of_string ~f2:Part.of_string)
  ;;

  let for_chop_prefix =
    strings_for_chop_prefix |> List.map ~f:(With_prefix.map ~f:of_string)
  ;;

  let for_chop_suffix =
    strings_for_chop_suffix |> List.map ~f:(With_suffix.map ~f:of_string)
  ;;

  let for_append =
    strings_for_append |> List.map ~f:(pair_map ~f1:of_string ~f2:Relative.of_string)
  ;;

  let for_simplify = strings_for_simplify |> List.map ~f:of_string

  let for_make_absolute =
    strings_for_make_absolute |> List.map ~f:(With_under.map ~f:of_string)
  ;;

  let for_make_relative =
    strings_for_make_relative |> List.map ~f:(With_if_under.map ~f:of_string)
  ;;

  let variant_for_conversion =
    List.map strings_for_variant ~f:(fun string : File_path.Variant.t ->
      if String.is_prefix string ~prefix:"/"
      then Absolute (Absolute.of_string string)
      else Relative (Relative.of_string string))
  ;;
end
