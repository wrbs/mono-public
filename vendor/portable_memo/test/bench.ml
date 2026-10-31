open Core

let[@inline always] bench_memo_fun_cache_hit ~memo =
  let n = 100_000 in
  for i = 0 to n - 1 do
    ignore (memo i : int)
  done;
  let r = ref 0 in
  fun () ->
    let i = !r % n in
    ignore (Sys.opaque_identity (memo i) : int);
    incr r
;;

let%bench_fun "read" =
  let memo = Portable_memo.create (module Int) in
  let n = 100_000 in
  for i = 0 to n - 1 do
    ignore (Portable_memo.add memo ~key:i ~data:i : int)
  done;
  let r = ref 0 in
  fun () ->
    let i = !r % n in
    (match Portable_memo.find memo i with
     | None -> assert false
     | Some i -> ignore (Sys.opaque_identity i : int));
    incr r
;;

let%bench_fun "add" =
  let memo = Portable_memo.create (module Int) in
  let n = 100_000 in
  for i = 0 to n - 1 do
    ignore (Portable_memo.add memo ~key:i ~data:i : int)
  done;
  let r = ref 0 in
  fun () ->
    let i = !r in
    let _ : int = Portable_memo.add memo ~key:(n + i) ~data:i in
    incr r
;;

let%bench_fun "memo cache hit" =
  let memo = Portable_memo.memo (module Int) (fun (i : int) -> Sys.opaque_identity i) in
  bench_memo_fun_cache_hit ~memo
;;

let%bench_fun "Hashtbl.find" =
  let memo = Int.Table.create () in
  let n = 100_000 in
  for i = 0 to n - 1 do
    ignore (Hashtbl.add_exn memo ~key:i ~data:i : _)
  done;
  let r = ref 0 in
  fun () ->
    let i = !r % n in
    (match Hashtbl.find memo i with
     | None -> assert false
     | Some i -> ignore (Sys.opaque_identity i : int));
    incr r
;;

let%bench_fun "Hashtbl.add_exn" =
  let memo = Int.Table.create () in
  let n = 100_000 in
  for i = 0 to n - 1 do
    ignore (Hashtbl.add_exn memo ~key:i ~data:i : _)
  done;
  let r = ref 0 in
  fun () ->
    let i = !r in
    Hashtbl.add_exn memo ~key:(n + i) ~data:i;
    incr r
;;

let%bench_fun "Memo.general cache hit" =
  let memo =
    Memo.general ~hashable:Int.hashable (fun (i : int) -> Sys.opaque_identity i)
  in
  bench_memo_fun_cache_hit ~memo
;;
