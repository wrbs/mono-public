open! Core
module Bonsai = Bonsai.Cont
open Bonsai.Let_syntax

let fallback =
  (* pick weird but not insane (so it sounds quarterish speed/2ish octaves down
  and has a greppable value, but doesn't break everything)
  
  every driver should ensure actual sample rate set in practice *)
  12345
;;

let dynamic_scope =
  Bonsai.Dynamic_scope.create ~name:"sample-rate" ~fallback ~sexp_of:[%sexp_of: int] ()
;;

let int graph = Bonsai.Dynamic_scope.lookup dynamic_scope graph
let float graph = int graph >>| Float.of_int

let sample_length_sec graph =
  let%arr x = float graph in
  1. /. x
;;

let sample_length graph = sample_length_sec graph >>| Time_ns.Span.of_sec

let block_length_sec graph =
  let%arr x = float graph in
  Float.of_int Block.size /. x
;;

let block_length graph = block_length_sec graph >>| Time_ns.Span.of_sec

module Expert = struct
  let dynamic_scope = dynamic_scope
end
