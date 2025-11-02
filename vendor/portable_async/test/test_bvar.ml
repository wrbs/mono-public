open! Core
open! Async
open Portable_test_helpers

let%expect_test "fill ivar from another domain" =
  let bvar = Portable_async.Bvar.create () in
  let barrier = Barrier.create 2 in
  (match
     Multicore.spawn
       (fun () ->
         Barrier.await barrier;
         Portable_async.Bvar.broadcast bvar "Hello from another domain!")
       ()
   with
   | Spawned -> ()
   | Failed ((), exn, bt) -> Exn.raise_with_original_backtrace exn bt);
  let result = Portable_async.Bvar.wait bvar in
  Barrier.await barrier;
  let%bind result in
  print_endline result;
  [%expect {| Hello from another domain! |}];
  return ()
;;
