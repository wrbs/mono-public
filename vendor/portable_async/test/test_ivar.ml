open! Core
open! Async

let%expect_test "fill ivar from another domain" =
  let ivar = Portable_async.Ivar.create () in
  (match
     Multicore.spawn
       (fun () -> Portable_async.Ivar.fill_if_empty ivar "Hello from another domain!")
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  let%bind result = Portable_async.Ivar.read ivar in
  print_s [%sexp (result : string)];
  [%expect {| "Hello from another domain!" |}];
  return ()
;;
