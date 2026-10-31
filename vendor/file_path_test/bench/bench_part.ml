(* See comment in [bench_path.ml]. *)

open! Core

let dot = File_path.Part.dot
let dot_dot = File_path.Part.dot_dot

include Bench_common.Part

[%%template
[@@@alloc a @ l = (stack_local, heap_global)]

let[@alloc a] append_to_basename_exn = (File_path.Part.append_to_basename_exn [@alloc a])

let%bench_fun "append_to_basename_exn, empty" =
  let t = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity "" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;

let%bench_fun "append_to_basename_exn, nonempty" =
  let t = Sys.opaque_identity (of_string "foo") in
  let suffix = Sys.opaque_identity ".bar" in
  fun () -> ignore ((append_to_basename_exn [@alloc a]) t suffix : t)
;;]
