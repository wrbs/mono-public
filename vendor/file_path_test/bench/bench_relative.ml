(* See comment in [bench_path.ml]. *)

open! Core

let dot = File_path.Relative.dot
let dot_dot = File_path.Relative.dot_dot

include Bench_common.Relative

let is_prefix = File_path.Relative.is_prefix

let%bench_fun "is_prefix, true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> is_prefix t ~prefix
;;

let is_suffix = File_path.Relative.is_suffix

let%bench_fun "is_suffix, true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let number_of_parts = File_path.Relative.number_of_parts

let%bench_fun "number_of_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> number_of_parts t
;;

[%%template
[@@@alloc a @ l = (stack_local, heap_global)]

let[@mode l] of_part = (File_path.Relative.of_part [@mode l])

let%bench_fun "of_part" =
  let part = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> ignore ((of_part [@mode l]) part : t)
;;

let[@alloc a] basename = (File_path.Relative.basename [@alloc a])

let%bench_fun "basename" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] dirname = (File_path.Relative.dirname [@alloc a])

let%bench_fun "dirname, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let%bench_fun "dirname, none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let[@alloc a] dirname_exn = (File_path.Relative.dirname_exn [@alloc a])

let%bench_fun "dirname_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_exn [@alloc a]) t : t)
;;

let[@alloc a] dirname_or_error = (File_path.Relative.dirname_or_error [@alloc a])

let%bench_fun "dirname_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_or_error [@alloc a]) t : t Or_error.t)
;;

let[@alloc a] dirname_defaulting_to_dot =
  (File_path.Relative.dirname_defaulting_to_dot [@alloc a])
;;

let%bench_fun "dirname_defaulting_to_dot, dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_defaulting_to_dot [@alloc a]) t : t)
;;

let%bench_fun "dirname_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> ignore ((dirname_defaulting_to_dot [@alloc a]) t : t)
;;

let[@alloc a] top_dir = (File_path.Relative.top_dir [@alloc a])

let%bench_fun "top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((top_dir [@alloc a]) t : File_path.Part.t option)
;;

let%bench_fun "top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((top_dir [@alloc a]) t : File_path.Part.t option)
;;

let[@alloc a] top_dir_exn = (File_path.Relative.top_dir_exn [@alloc a])

let%bench_fun "top_dir_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((top_dir_exn [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] top_dir_or_error = (File_path.Relative.top_dir_or_error [@alloc a])

let%bench_fun "top_dir_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((top_dir_or_error [@alloc a]) t : File_path.Part.t Or_error.t)
;;

let[@alloc a] top_dir_defaulting_to_dot =
  (File_path.Relative.top_dir_defaulting_to_dot [@alloc a])
;;

let%bench_fun "top_dir_defaulting_to_dot, dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((top_dir_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let%bench_fun "top_dir_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((top_dir_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] all_but_top_dir = (File_path.Relative.all_but_top_dir [@alloc a])

let%bench_fun "all_but_top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((all_but_top_dir [@alloc a]) t : t option)
;;

let%bench_fun "all_but_top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((all_but_top_dir [@alloc a]) t : t option)
;;

let[@alloc a] all_but_top_dir_exn = (File_path.Relative.all_but_top_dir_exn [@alloc a])

let%bench_fun "all_but_top_dir_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((all_but_top_dir_exn [@alloc a]) t : t)
;;

let[@alloc a] all_but_top_dir_or_error =
  (File_path.Relative.all_but_top_dir_or_error [@alloc a])
;;

let%bench_fun "all_but_top_dir_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((all_but_top_dir_or_error [@alloc a]) t : t Or_error.t)
;;

let[@alloc a] all_but_top_dir_defaulting_to_self =
  (File_path.Relative.all_but_top_dir_defaulting_to_self [@alloc a])
;;

let%bench_fun "all_but_top_dir_defaulting_to_self, path" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((all_but_top_dir_defaulting_to_self [@alloc a]) t : t)
;;

let%bench_fun "all_but_top_dir_defaulting_to_self, self" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((all_but_top_dir_defaulting_to_self [@alloc a]) t : t)
;;

let[@alloc a] top_dir_and_all_but_top_dir =
  (File_path.Relative.top_dir_and_all_but_top_dir [@alloc a])
;;

let%bench_fun "top_dir_and_all_but_top_dir, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () ->
    ignore ((top_dir_and_all_but_top_dir [@alloc a]) t : (File_path.Part.t * t) option)
;;

let%bench_fun "top_dir_and_all_but_top_dir, none" =
  let t = Sys.opaque_identity dot in
  fun () ->
    ignore ((top_dir_and_all_but_top_dir [@alloc a]) t : (File_path.Part.t * t) option)
;;

let[@alloc a] append_to_basename_exn =
  (File_path.Relative.append_to_basename_exn [@alloc a])
;;

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let[@alloc a] append_part = (File_path.Relative.append_part [@alloc a])

let%bench_fun "append_part" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let[@alloc a] prepend_part = (File_path.Relative.prepend_part [@alloc a])

let%bench_fun "prepend_part" =
  let prefix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  let t = Sys.opaque_identity (of_string "bar/baz") in
  fun () -> ignore ((prepend_part [@alloc a]) prefix t : t)
;;

let[@alloc a] chop_prefix = (File_path.Relative.chop_prefix [@alloc a])

let%bench_fun "chop_prefix, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : t option)
;;

let%bench_fun "chop_prefix, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : t option)
;;

let[@alloc a] chop_prefix_exn = (File_path.Relative.chop_prefix_exn [@alloc a])

let%bench_fun "chop_prefix_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix_exn [@alloc a]) t ~prefix : t)
;;

let[@alloc a] chop_prefix_or_error = (File_path.Relative.chop_prefix_or_error [@alloc a])

let%bench_fun "chop_prefix_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix_or_error [@alloc a]) t ~prefix : t Or_error.t)
;;

let[@alloc a] chop_prefix_if_exists =
  (File_path.Relative.chop_prefix_if_exists [@alloc a])
;;

let%bench_fun "chop_prefix_if_exists" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix_if_exists [@alloc a]) t ~prefix : t)
;;

let[@alloc a] chop_suffix = (File_path.Relative.chop_suffix [@alloc a])

let%bench_fun "chop_suffix, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let%bench_fun "chop_suffix, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let[@alloc a] chop_suffix_exn = (File_path.Relative.chop_suffix_exn [@alloc a])

let%bench_fun "chop_suffix_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_exn [@alloc a]) t ~suffix : t)
;;

let[@alloc a] chop_suffix_or_error = (File_path.Relative.chop_suffix_or_error [@alloc a])

let%bench_fun "chop_suffix_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_or_error [@alloc a]) t ~suffix : t Or_error.t)
;;

let[@alloc a] chop_suffix_if_exists =
  (File_path.Relative.chop_suffix_if_exists [@alloc a])
;;

let%bench_fun "chop_suffix_if_exists" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_if_exists [@alloc a]) t ~suffix : t)
;;

let[@alloc a] append = (File_path.Relative.append [@alloc a])

let%bench_fun "append" =
  let prefix = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let[@alloc a] to_parts = (File_path.Relative.to_parts [@alloc a])

let%bench_fun "to_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let%bench_fun "to_parts, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let[@alloc a] to_parts_nonempty = (File_path.Relative.to_parts_nonempty [@alloc a])

let%bench_fun "to_parts_nonempty, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((to_parts_nonempty [@alloc a]) t : File_path.Part.t Nonempty_list.t)
;;

let%bench_fun "to_parts_nonempty, compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_parts_nonempty [@alloc a]) t : File_path.Part.t Nonempty_list.t)
;;

let[@alloc a] of_parts = (File_path.Relative.of_parts [@alloc a])

let%bench_fun "of_parts, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> ignore ((of_parts [@alloc a]) parts : t option)
;;

let%bench_fun "of_parts, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts [@alloc a]) parts : t option)
;;

let[@alloc a] of_parts_exn = (File_path.Relative.of_parts_exn [@alloc a])

let%bench_fun "of_parts_exn" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_exn [@alloc a]) parts : t)
;;

let[@alloc a] of_parts_or_error = (File_path.Relative.of_parts_or_error [@alloc a])

let%bench_fun "of_parts_or_error" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_or_error [@alloc a]) parts : t Or_error.t)
;;

let[@alloc a] of_parts_defaulting_to_dot =
  (File_path.Relative.of_parts_defaulting_to_dot [@alloc a])
;;

let%bench_fun "of_parts_defaulting_to_dot, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> ignore ((of_parts_defaulting_to_dot [@alloc a]) parts : t)
;;

let%bench_fun "of_parts_defaulting_to_dot, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_defaulting_to_dot [@alloc a]) parts : t)
;;

let[@alloc a] of_parts_nonempty = (File_path.Relative.of_parts_nonempty [@alloc a])

let%bench_fun "of_parts_nonempty" =
  let parts =
    Sys.opaque_identity
      ([ File_path.Part.of_string "foo"
       ; File_path.Part.of_string "bar"
       ; File_path.Part.of_string "baz"
       ]
       : _ Nonempty_list.t)
  in
  fun () -> ignore ((of_parts_nonempty [@alloc a]) parts : t)
;;

let[@alloc a] simplify_dot = (File_path.Relative.simplify_dot [@alloc a])

let%bench_fun "simplify_dot, unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot, changed" =
  let t = Sys.opaque_identity (of_string "./foo/./bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let[@alloc a] simplify_dot_and_dot_dot_naively =
  (File_path.Relative.simplify_dot_and_dot_dot_naively [@alloc a])
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, changed" =
  let t = Sys.opaque_identity (of_string "./foo/quux/../bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;]
