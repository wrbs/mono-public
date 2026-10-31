(* See [test_exn_backtrace.ml] on why this file exists *)

open Hardcaml_lws

val raise_and_die : Lws.Handler.t @ local -> unit
