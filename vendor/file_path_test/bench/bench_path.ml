(* The [File_path] library has been constructed carefully with respect to performance,
   especially allocation. We benchmark (nearly) every function so we can check features
   for performance improvements / regressions.

   We constrain these modules to the interface of [File_path] and its submodules to make
   sure we have benchmarked all appropriate bindings. Whenever a new binding in
   [File_path] forces us to add a new binding here, we should add a new benchmark unless
   there is a pressing reason not to.

   For ppx-derived and functor-generated bindings, we test only a few functions. Otherwise
   we test every function. Where there are multiple cases, we try to benchmark all
   meaningfully different paths that do not raise.

   We define all benchmarks using [let%bench_fun], and bind all function arguments outside
   the final closure, wrapped in [Sys.opaque_identity]. This guarantees that computing the
   argument is not part of what we time, and that the closure cannot be specialized to the
   argument value. *)

open! Core

let root = File_path.root
let dot = File_path.dot
let dot_dot = File_path.dot_dot

include Bench_common.Path

let is_relative = File_path.is_relative

let%bench_fun "is_relative" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> is_relative t
;;

let is_absolute = File_path.is_absolute

let%bench_fun "is_absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> is_absolute t
;;

let is_prefix = File_path.is_prefix

let%bench_fun "is_prefix, absolute true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, absolute false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, relative true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> is_prefix t ~prefix
;;

let%bench_fun "is_prefix, relative false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> is_prefix t ~prefix
;;

let is_suffix = File_path.is_suffix

let%bench_fun "is_suffix, absolute true" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, absolute false" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, relative true" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> is_suffix t ~suffix
;;

let%bench_fun "is_suffix, relative false" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> is_suffix t ~suffix
;;

let number_of_parts = File_path.number_of_parts

let%bench_fun "number_of_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, absolute compound" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> number_of_parts t
;;

let%bench_fun "number_of_parts, relative compound" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> number_of_parts t
;;

module Variant = File_path.Variant

[%%template
[@@@alloc a @ l = (stack_local, heap_global)]

let[@alloc a] to_variant = (File_path.to_variant [@alloc a])

let%bench_fun "to_variant, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_variant [@alloc a]) t : Variant.t)
;;

let%bench_fun "to_variant, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_variant [@alloc a]) t : Variant.t)
;;

let[@mode l] of_variant = (File_path.of_variant [@mode l])

let%bench_fun "of_variant, absolute" =
  let variant =
    Sys.opaque_identity (Variant.Absolute (File_path.Absolute.of_string "/foo/bar/baz"))
  in
  fun () -> ignore ((of_variant [@mode l]) variant : t)
;;

let%bench_fun "of_variant, relative" =
  let variant =
    Sys.opaque_identity (Variant.Relative (File_path.Relative.of_string "foo/bar/baz"))
  in
  fun () -> ignore ((of_variant [@mode l]) variant : t)
;;

let[@alloc a] to_absolute = (File_path.to_absolute [@alloc a])

let%bench_fun "to_absolute, some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_absolute [@alloc a]) t : File_path.Absolute.t option)
;;

let%bench_fun "to_absolute, none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_absolute [@alloc a]) t : File_path.Absolute.t option)
;;

let[@alloc a] to_relative = (File_path.to_relative [@alloc a])

let%bench_fun "to_relative, some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_relative [@alloc a]) t : File_path.Relative.t option)
;;

let%bench_fun "to_relative, none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_relative [@alloc a]) t : File_path.Relative.t option)
;;

let[@mode l] to_absolute_exn = (File_path.to_absolute_exn [@mode l])

let%bench_fun "to_absolute_exn" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_absolute_exn [@mode l]) t : File_path.Absolute.t)
;;

let[@mode l] to_relative_exn = (File_path.to_relative_exn [@mode l])

let%bench_fun "to_relative_exn" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_relative_exn [@mode l]) t : File_path.Relative.t)
;;

let[@alloc a] to_absolute_or_error = (File_path.to_absolute_or_error [@alloc a])

let%bench_fun "to_absolute_or_error" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_absolute_or_error [@alloc a]) t : File_path.Absolute.t Or_error.t)
;;

let[@alloc a] to_relative_or_error = (File_path.to_relative_or_error [@alloc a])

let%bench_fun "to_relative_or_error" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_relative_or_error [@alloc a]) t : File_path.Relative.t Or_error.t)
;;

let[@mode l] of_absolute = (File_path.of_absolute [@mode l])

let%bench_fun "of_absolute" =
  let absolute = Sys.opaque_identity (File_path.Absolute.of_string "/foo/bar/baz") in
  fun () -> ignore ((of_absolute [@mode l]) absolute : t)
;;

let[@mode l] of_relative = (File_path.of_relative [@mode l])

let%bench_fun "of_relative" =
  let relative = Sys.opaque_identity (File_path.Relative.of_string "foo/bar/baz") in
  fun () -> ignore ((of_relative [@mode l]) relative : t)
;;

let[@mode l] of_part_relative = (File_path.of_part_relative [@mode l])

let%bench_fun "of_part_relative" =
  let part = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> ignore ((of_part_relative [@mode l]) part : t)
;;

let[@alloc a] basename = (File_path.basename [@alloc a])

let%bench_fun "basename, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t option)
;;

let%bench_fun "basename, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t option)
;;

let%bench_fun "basename, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((basename [@alloc a]) t : File_path.Part.t option)
;;

let[@alloc a] basename_exn = (File_path.basename_exn [@alloc a])

let%bench_fun "basename_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_exn [@alloc a]) t : File_path.Part.t)
;;

let%bench_fun "basename_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((basename_exn [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] basename_or_error = (File_path.basename_or_error [@alloc a])

let%bench_fun "basename_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_or_error [@alloc a]) t : File_path.Part.t Or_error.t)
;;

let%bench_fun "basename_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((basename_or_error [@alloc a]) t : File_path.Part.t Or_error.t)
;;

let[@alloc a] basename_defaulting_to_dot =
  (File_path.basename_defaulting_to_dot [@alloc a])
;;

let%bench_fun "basename_defaulting_to_dot, absolute name" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((basename_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let%bench_fun "basename_defaulting_to_dot, absolute dot" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((basename_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let%bench_fun "basename_defaulting_to_dot, relative name" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((basename_defaulting_to_dot [@alloc a]) t : File_path.Part.t)
;;

let[@alloc a] dirname = (File_path.dirname [@alloc a])

let%bench_fun "dirname, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let%bench_fun "dirname, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let%bench_fun "dirname, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let%bench_fun "dirname, relative none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> ignore ((dirname [@alloc a]) t : t option)
;;

let[@alloc a] dirname_exn = (File_path.dirname_exn [@alloc a])

let%bench_fun "dirname_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_exn [@alloc a]) t : t)
;;

let%bench_fun "dirname_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_exn [@alloc a]) t : t)
;;

let[@alloc a] dirname_or_error = (File_path.dirname_or_error [@alloc a])

let%bench_fun "dirname_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_or_error [@alloc a]) t : t Or_error.t)
;;

let%bench_fun "dirname_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_or_error [@alloc a]) t : t Or_error.t)
;;

let[@alloc a] dirname_defaulting_to_dot_or_root =
  (File_path.dirname_defaulting_to_dot_or_root [@alloc a])
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, absolute dir" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_defaulting_to_dot_or_root [@alloc a]) t : t)
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, absolute root" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname_defaulting_to_dot_or_root [@alloc a]) t : t)
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, relative dir" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_defaulting_to_dot_or_root [@alloc a]) t : t)
;;

let%bench_fun "dirname_defaulting_to_dot_or_root, relative dot" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> ignore ((dirname_defaulting_to_dot_or_root [@alloc a]) t : t)
;;

let[@alloc a] dirname_and_basename = (File_path.dirname_and_basename [@alloc a])

let%bench_fun "dirname_and_basename, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let%bench_fun "dirname_and_basename, absolute none" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let%bench_fun "dirname_and_basename, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let%bench_fun "dirname_and_basename, relative none" =
  let t = Sys.opaque_identity (of_string "foo.bar.baz") in
  fun () -> ignore ((dirname_and_basename [@alloc a]) t : (t * File_path.Part.t) option)
;;

let[@alloc a] append_to_basename_exn = (File_path.append_to_basename_exn [@alloc a])

let%bench_fun "append_to_basename_exn, absolute, empty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, absolute, nonempty" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, relative, empty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity "" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, relative, nonempty" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity ".baz" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let[@alloc a] append_part = (File_path.append_part [@alloc a])

let%bench_fun "append_part, root" =
  let t = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "foo") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let%bench_fun "append_part, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let%bench_fun "append_part, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar") in
  let suffix = Sys.opaque_identity (File_path.Part.of_string "baz") in
  fun () -> ignore ((append_part [@alloc a]) t suffix : t)
;;

let[@alloc a] chop_prefix = (File_path.chop_prefix [@alloc a])

let%bench_fun "chop_prefix, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let%bench_fun "chop_prefix, absolute none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foobie") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let%bench_fun "chop_prefix, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let%bench_fun "chop_prefix, relative none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foobie") in
  fun () -> ignore ((chop_prefix [@alloc a]) t ~prefix : File_path.Relative.t option)
;;

let[@alloc a] chop_prefix_exn = (File_path.chop_prefix_exn [@alloc a])

let%bench_fun "chop_prefix_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> ignore ((chop_prefix_exn [@alloc a]) t ~prefix : File_path.Relative.t)
;;

let%bench_fun "chop_prefix_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix_exn [@alloc a]) t ~prefix : File_path.Relative.t)
;;

let[@alloc a] chop_prefix_or_error = (File_path.chop_prefix_or_error [@alloc a])

let%bench_fun "chop_prefix_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () ->
    ignore ((chop_prefix_or_error [@alloc a]) t ~prefix : File_path.Relative.t Or_error.t)
;;

let%bench_fun "chop_prefix_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () ->
    ignore ((chop_prefix_or_error [@alloc a]) t ~prefix : File_path.Relative.t Or_error.t)
;;

let[@alloc a] chop_prefix_if_exists = (File_path.chop_prefix_if_exists [@alloc a])

let%bench_fun "chop_prefix_if_exists, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "/foo/bar") in
  fun () -> ignore ((chop_prefix_if_exists [@alloc a]) t ~prefix : t)
;;

let%bench_fun "chop_prefix_if_exists, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let prefix = Sys.opaque_identity (of_string "foo/bar") in
  fun () -> ignore ((chop_prefix_if_exists [@alloc a]) t ~prefix : t)
;;

let[@alloc a] chop_suffix = (File_path.chop_suffix [@alloc a])

let%bench_fun "chop_suffix, absolute some" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let%bench_fun "chop_suffix, absolute none" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let%bench_fun "chop_suffix, relative some" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let%bench_fun "chop_suffix, relative none" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "barbie") in
  fun () -> ignore ((chop_suffix [@alloc a]) t ~suffix : t option)
;;

let[@alloc a] chop_suffix_exn = (File_path.chop_suffix_exn [@alloc a])

let%bench_fun "chop_suffix_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_exn [@alloc a]) t ~suffix : t)
;;

let%bench_fun "chop_suffix_exn, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_exn [@alloc a]) t ~suffix : t)
;;

let[@alloc a] chop_suffix_or_error = (File_path.chop_suffix_or_error [@alloc a])

let%bench_fun "chop_suffix_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_or_error [@alloc a]) t ~suffix : t Or_error.t)
;;

let%bench_fun "chop_suffix_or_error, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_or_error [@alloc a]) t ~suffix : t Or_error.t)
;;

let[@alloc a] chop_suffix_if_exists = (File_path.chop_suffix_if_exists [@alloc a])

let%bench_fun "chop_suffix_if_exists, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_if_exists [@alloc a]) t ~suffix : t)
;;

let%bench_fun "chop_suffix_if_exists, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((chop_suffix_if_exists [@alloc a]) t ~suffix : t)
;;

let[@alloc a] append = (File_path.append [@alloc a])

let%bench_fun "append, root" =
  let prefix = Sys.opaque_identity root in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let%bench_fun "append, absolute" =
  let prefix = Sys.opaque_identity (of_string "/foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let%bench_fun "append, relative" =
  let prefix = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity (File_path.Relative.of_string "bar/baz") in
  fun () -> ignore ((append [@alloc a]) prefix suffix : t)
;;

let[@alloc a] to_parts = (File_path.to_parts [@alloc a])

let%bench_fun "to_parts, root" =
  let t = Sys.opaque_identity root in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let%bench_fun "to_parts, dot" =
  let t = Sys.opaque_identity dot in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let%bench_fun "to_parts, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let%bench_fun "to_parts, relative" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((to_parts [@alloc a]) t : File_path.Part.t list)
;;

let[@alloc a] of_parts_absolute = (File_path.of_parts_absolute [@alloc a])

let%bench_fun "of_parts_absolute" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_absolute [@alloc a]) parts : t)
;;

let[@alloc a] of_parts_relative = (File_path.of_parts_relative [@alloc a])

let%bench_fun "of_parts_relative, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> ignore ((of_parts_relative [@alloc a]) parts : t option)
;;

let%bench_fun "of_parts_relative, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_relative [@alloc a]) parts : t option)
;;

let[@alloc a] of_parts_relative_exn = (File_path.of_parts_relative_exn [@alloc a])

let%bench_fun "of_parts_relative_exn" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_relative_exn [@alloc a]) parts : t)
;;

let[@alloc a] of_parts_relative_or_error =
  (File_path.of_parts_relative_or_error [@alloc a])
;;

let%bench_fun "of_parts_relative_or_error" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_relative_or_error [@alloc a]) parts : t Or_error.t)
;;

let[@alloc a] of_parts_relative_defaulting_to_dot =
  (File_path.of_parts_relative_defaulting_to_dot [@alloc a])
;;

let%bench_fun "of_parts_relative_defaulting_to_dot, empty" =
  let parts = Sys.opaque_identity [] in
  fun () -> ignore ((of_parts_relative_defaulting_to_dot [@alloc a]) parts : t)
;;

let%bench_fun "of_parts_relative_defaulting_to_dot, non-empty" =
  let parts =
    Sys.opaque_identity
      [ File_path.Part.of_string "foo"
      ; File_path.Part.of_string "bar"
      ; File_path.Part.of_string "baz"
      ]
  in
  fun () -> ignore ((of_parts_relative_defaulting_to_dot [@alloc a]) parts : t)
;;

let[@alloc a] of_parts_relative_nonempty =
  (File_path.of_parts_relative_nonempty [@alloc a])
;;

let%bench_fun "of_parts_relative_nonempty" =
  let parts =
    Sys.opaque_identity
      ([ File_path.Part.of_string "foo"
       ; File_path.Part.of_string "bar"
       ; File_path.Part.of_string "baz"
       ]
       : _ Nonempty_list.t)
  in
  fun () -> ignore ((of_parts_relative_nonempty [@alloc a]) parts : t)
;;

let[@alloc a] make_absolute = (File_path.make_absolute [@alloc a])

let%bench_fun "make_absolute, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_absolute [@alloc a]) t ~under : File_path.Absolute.t)
;;

let%bench_fun "make_absolute, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_absolute [@alloc a]) t ~under : File_path.Absolute.t)
;;

let[@alloc a] make_relative = (File_path.make_relative [@alloc a])

let%bench_fun "make_relative, absolute under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative [@alloc a]) t ~if_under : File_path.Relative.t option)
;;

let%bench_fun "make_relative, absolute not-under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foobie") in
  fun () -> ignore ((make_relative [@alloc a]) t ~if_under : File_path.Relative.t option)
;;

let%bench_fun "make_relative, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative [@alloc a]) t ~if_under : File_path.Relative.t option)
;;

let[@alloc a] make_relative_exn = (File_path.make_relative_exn [@alloc a])

let%bench_fun "make_relative_exn, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative_exn [@alloc a]) t ~if_under : File_path.Relative.t)
;;

let%bench_fun "make_relative_exn, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative_exn [@alloc a]) t ~if_under : File_path.Relative.t)
;;

let[@alloc a] make_relative_or_error = (File_path.make_relative_or_error [@alloc a])

let%bench_fun "make_relative_or_error, absolute" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () ->
    ignore
      ((make_relative_or_error [@alloc a]) t ~if_under : File_path.Relative.t Or_error.t)
;;

let%bench_fun "make_relative_or_error, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () ->
    ignore
      ((make_relative_or_error [@alloc a]) t ~if_under : File_path.Relative.t Or_error.t)
;;

let[@alloc a] make_relative_if_possible = (File_path.make_relative_if_possible [@alloc a])

let%bench_fun "make_relative_if_possible, absolute under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative_if_possible [@alloc a]) t ~if_under : t)
;;

let%bench_fun "make_relative_if_possible, absolute not-under" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foobie") in
  fun () -> ignore ((make_relative_if_possible [@alloc a]) t ~if_under : t)
;;

let%bench_fun "make_relative_if_possible, relative" =
  let t = Sys.opaque_identity (of_string "bar/baz") in
  let if_under = Sys.opaque_identity (File_path.Absolute.of_string "/foo") in
  fun () -> ignore ((make_relative_if_possible [@alloc a]) t ~if_under : t)
;;

let[@alloc a] simplify_dot = (File_path.simplify_dot [@alloc a])

let%bench_fun "simplify_dot, absolute unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot, relative unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot, absolute changed" =
  let t = Sys.opaque_identity (of_string "/foo/./bar/baz/.") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot, relative changed" =
  let t = Sys.opaque_identity (of_string "./foo/./bar/baz") in
  fun () -> ignore ((simplify_dot [@alloc a]) t : t)
;;

let[@alloc a] simplify_dot_and_dot_dot_naively =
  (File_path.simplify_dot_and_dot_dot_naively [@alloc a])
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, absolute unchanged" =
  let t = Sys.opaque_identity (of_string "/foo/bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, relative unchanged" =
  let t = Sys.opaque_identity (of_string "foo/bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, absolute changed" =
  let t = Sys.opaque_identity (of_string "/foo/quux/../bar/baz/.") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;

let%bench_fun "simplify_dot_and_dot_dot_naively, relative changed" =
  let t = Sys.opaque_identity (of_string "./foo/quux/../bar/baz") in
  fun () -> ignore ((simplify_dot_and_dot_dot_naively [@alloc a]) t : t)
;;]
