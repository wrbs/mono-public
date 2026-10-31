open! Core

(* The operators are synonyms for already-tested functions. *)

module%template [@alloc a @ l = (stack_local, heap_global)] O = struct
  let ( ~/ ) = (Test_relative.of_string [@alloc a])
  let ( !/ ) = (Test_absolute.of_string [@alloc a])
  let ( ?/ ) = (Test_path.of_string [@alloc a])
  let ( ~. ) = (Test_part.of_string [@alloc a])
  let ( !/$ ) = (Test_absolute.to_string [@alloc a])
  let ( ~/$ ) = (Test_relative.to_string [@alloc a])
  let ( ?/$ ) = (Test_path.to_string [@alloc a])
  let ( ~.$ ) = (Test_part.to_string [@alloc a])
  let ( !/? ) = (Test_path.of_absolute [@mode l])
  let ( ~/? ) = (Test_path.of_relative [@mode l])
  let ( ~.? ) = (Test_path.of_part_relative [@mode l])
  let ( ~.~ ) = (Test_relative.of_part [@mode l])
  let ( /~/ ) = (Test_relative.append [@alloc a])
  let ( /!/ ) = (Test_absolute.append [@alloc a])
  let ( /?/ ) = (Test_path.append [@alloc a])
  let ( /~. ) = (Test_relative.append_part [@alloc a])
  let ( /!. ) = (Test_absolute.append_part [@alloc a])
  let ( /?. ) = (Test_path.append_part [@alloc a])
  let ( /~^ ) = (Test_relative.append_to_basename_exn [@alloc a])
  let ( /!^ ) = (Test_absolute.append_to_basename_exn [@alloc a])
  let ( /?^ ) = (Test_path.append_to_basename_exn [@alloc a])
  let ( /.^ ) = (Test_part.append_to_basename_exn [@alloc a])
end
