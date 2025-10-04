open! Core

(* exports *)
module Block = Block
module Driver = Driver
module Uptime = Uptime
module Sample_rate = Sample_rate

(* re-exports *)
module Bonsai = Bonsai.Cont
module Effect = Bonsai.Effect

(* global helpers *)
open Bonsai.Let_syntax

let after_tick f graph = Bonsai.Edge.after_display f graph
let after_tick' f graph = Bonsai.Edge.after_display' f graph

let stateful f ~init graph =
  let state, set_state = Bonsai.state init graph in
  let%sub result, set_next_state =
    let%arr f = f
    and state = state
    and set_state = set_state in
    let next_state, result = f state in
    result, set_state next_state
  in
  after_tick set_next_state graph;
  result
;;

let ( +| ) = Bonsai.map2 ~f:Block.O.( + )
let ( -| ) = Bonsai.map2 ~f:Block.O.( - )
let ( *| ) = Bonsai.map2 ~f:Block.O.( * )
let ( /| ) = Bonsai.map2 ~f:Block.O.( / )
let ( +.| ) = Bonsai.map2 ~f:Block.O.( +. )
let ( -.| ) = Bonsai.map2 ~f:Block.O.( +. )
let ( *.| ) = Bonsai.map2 ~f:Block.O.( *. )
let ( /.| ) = Bonsai.map2 ~f:Block.O.( /. )
