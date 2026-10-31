open Core
open Hardcaml_lws

(* This is marked as cold to avoid inlining, so we get real backtraces *)
let[@cold] raise_and_die (h @ local) =
  let () = failwith "I AM DED!" in
  Lws.step ~n:2 h;
  printf "I should never be printed\n"
;;
