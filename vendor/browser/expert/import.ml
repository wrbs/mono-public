module Js = Js_of_ocaml.Js
module Types = Browser_types
module Context = Browser_manually_written_bindings.Context

let am_running_test =
  try
    ignore (Stdlib.Sys.getenv "TESTING_FRAMEWORK" : string);
    true
  with
  | Stdlib.Not_found -> false
;;

let return_false () = false
