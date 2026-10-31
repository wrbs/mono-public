(* See comment in [bench_path.ml]. *)

open! Core

let root = File_path.Absolute.root

include Bench_common.Absolute

let is_prefix = File_path.Absolute.is_prefix

let%bench_fun "is_prefix, true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> is_prefix t ~prefix
;;

let is_suffix = File_path.Absolute.is_suffix

let%bench_fun "is_suffix, true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let number_of_parts = File_path.Absolute.number_of_parts

let%bench_fun "number_of_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, compound" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> number_of_parts t
;;

[%%template
[@@@alloc a @ l = (stack_local, heap_global)]

let[@alloc a] basename = (File_path.Absolute.basename [@alloc a])

let%bench_fun "basename, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t option)
;;

let%bench_fun "basename, none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t option)
;;

let[@alloc a] basename_exn = (File_path.Absolute.basename_exn [@alloc a])

let%bench_fun "basename_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_exn [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] basename_or_error = (File_path.Absolute.basename_or_error [@alloc a])

let%bench_fun "basename_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_or_error [@alloc a]) t : File_path.Part.t Or_error.t)
;;

let[@alloc a] basename_defaulting_to_dot =
  (File_path.Absolute.basename_defaulting_to_dot [@alloc a])
;;

let%bench_fun "basename_defaulting_to_dot, name" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let%bench_fun "basename_defaulting_to_dot, dot" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((basename_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] dirname = (File_path.Absolute.dirname [@alloc a])

let%bench_fun "dirname, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let%bench_fun "dirname, none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let[@alloc a] dirname_exn = (File_path.Absolute.dirname_exn [@alloc a])

let%bench_fun "dirname_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_exn [@alloc a]) t : t)
;;

let[@alloc a] dirname_or_error = (File_path.Absolute.dirname_or_error [@alloc a])

let%bench_fun "dirname_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_or_error [@alloc a]) t : t Or_error.t)
;;

let[@alloc a] dirname_defaulting_to_root =
  (File_path.Absolute.dirname_defaulting_to_root [@alloc a])
;;

let%bench_fun "dirname_defaulting_to_root, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_defaulting_to_root [@alloc a]) t : t)
;;

let%bench_fun "dirname_defaulting_to_root, root" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname_defaulting_to_root [@alloc a]) t : t)
;;

let[@alloc a] dirname_and_basename = (File_path.Absolute.dirname_and_basename [@alloc a])

let%bench_fun "dirname_and_basename, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let%bench_fun "dirname_and_basename, none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let[@alloc a] append_to_basename_exn =
  (File_path.Absolute.append_to_basename_exn [@alloc a])
;;

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let[@alloc a] append_part = (File_path.Absolute.append_part [@alloc a])

let%bench_fun "append_part, root" =
  let t = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let%bench_fun "append_part, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let[@alloc a] chop_prefix = (File_path.Absolute.chop_prefix [@alloc a])

let%bench_fun "chop_prefix, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let%bench_fun "chop_prefix, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let[@alloc a] chop_prefix_exn = (File_path.Absolute.chop_prefix_exn [@alloc a])

let%bench_fun "chop_prefix_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> ignore ((chop_prefix_exn [@alloc a]) t ~prefix : File_path.Relative.t)
;;

let[@alloc a] chop_prefix_or_error = (File_path.Absolute.chop_prefix_or_error [@alloc a])

let%bench_fun "chop_prefix_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () ->
    ignore ((chop_prefix_or_error [@alloc a]) t ~prefix : File_path.Relative.t Or_error.t)
;;

let[@alloc a] chop_suffix = (File_path.Absolute.chop_suffix [@alloc a])

let%bench_fun "chop_suffix, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let%bench_fun "chop_suffix, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let[@alloc a] chop_suffix_exn = (File_path.Absolute.chop_suffix_exn [@alloc a])

let%bench_fun "chop_suffix_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_exn [@alloc a]) t ~suffix : t)
;;

let[@alloc a] chop_suffix_or_error = (File_path.Absolute.chop_suffix_or_error [@alloc a])

let%bench_fun "chop_suffix_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_or_error [@alloc a]) t ~suffix : t Or_error.t)
;;

let[@alloc a] chop_suffix_if_exists =
  (File_path.Absolute.chop_suffix_if_exists [@alloc a])
;;

let%bench_fun "chop_suffix_if_exists" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_if_exists [@alloc a]) t ~suffix : t)
;;

let[@alloc a] append = (File_path.Absolute.append [@alloc a])

let%bench_fun "append, root" =
  let prefix = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let%bench_fun "append, dir" =
  let prefix = Sys.opaque_identity (of_string "/foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let[@alloc a] to_parts = (File_path.Absolute.to_parts [@alloc a])

let%bench_fun "to_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let%bench_fun "to_parts, dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let[@alloc a] of_parts = (File_path.Absolute.of_parts [@alloc a])

let%bench_fun "of_parts" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts [@alloc a]) parts : t)
;;

let[@alloc a] simplify_dot = (File_path.Absolute.simplify_dot [@alloc a])

let%bench_fun "simplify_dot, unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot, changed" =
  let t = Sys.opaque_identity (of_string "/foo/./bar/baz/.") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let[@alloc a] simplify_dot_and_dot_dot_naively =
  (File_path.Absolute.simplify_dot_and_dot_dot_naively [@alloc a])
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, changed" =
  let t = Sys.opaque_identity (of_string "/foo/quux/../bar/baz/.") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;]
