open! Core

(* The operators are synonyms for already-benchmarked functions. *)

module%template [@alloc a @ l = (heap_global, stack_local)] O = struct
  let ( ~/ ) = (Bench_relative.of_string [@alloc a])
  let ( !/ ) = (Bench_absolute.of_string [@alloc a])
  let ( ?/ ) = (Bench_path.of_string [@alloc a])
  let ( ~. ) = (Bench_part.of_string [@alloc a])
  let ( !/$ ) = (Bench_absolute.to_string [@alloc a])
  let ( ~/$ ) = (Bench_relative.to_string [@alloc a])
  let ( ?/$ ) = (Bench_path.to_string [@alloc a])
  let ( ~.$ ) = (Bench_part.to_string [@alloc a])
  let ( !/? ) = (Bench_path.of_absolute [@mode l])
  let ( ~/? ) = (Bench_path.of_relative [@mode l])
  let ( ~.? ) = (Bench_path.of_part_relative [@mode l])
  let ( ~.~ ) = (Bench_relative.of_part [@mode l])
  let ( /~/ ) = (Bench_relative.append [@alloc a])
  let ( /!/ ) = (Bench_absolute.append [@alloc a])
  let ( /?/ ) = (Bench_path.append [@alloc a])
  let ( /~. ) = (Bench_relative.append_part [@alloc a])
  let ( /!. ) = (Bench_absolute.append_part [@alloc a])
  let ( /?. ) = (Bench_path.append_part [@alloc a])
  let ( /~^ ) = (Bench_relative.append_to_basename_exn [@alloc a])
  let ( /!^ ) = (Bench_absolute.append_to_basename_exn [@alloc a])
  let ( /?^ ) = (Bench_path.append_to_basename_exn [@alloc a])
  let ( /.^ ) = (Bench_part.append_to_basename_exn [@alloc a])
end
