open! Core
open! Import

let am_i_within_dls_key =
  (* There's a strange tradeoff in [split_from_parent].

     If a new domain is spawned while we are in deep-inflexible-state (i.e. during
     [run_within]), our options are:
     1. The new domain (perhaps surprisingly) jumps back to normal state rather than
     retaining deep-inflexible state, OR
     2. The new domain never returns to normal state, even after [run_within] completes in
     the parent domain.

     Both of these seem undesirable.
     If there was a way to forbid spawning a domain within [f] that would solve this,
     but as of 2025-06 there is no obvious way to do that.
  *)
  let split_from_parent (a : bool) : unit -> _ = fun () -> a in
  Domain.Safe.DLS.new_key ~split_from_parent (fun () -> false)
;;

let am_i_within () = Domain.Safe.DLS.get am_i_within_dls_key
let set value = Domain.Safe.DLS.set am_i_within_dls_key value

let run_within ~f =
  let prev = am_i_within () in
  set true;
  Exn.protect ~f ~finally:(stack_ fun () -> set prev) [@nontail]
;;
