open! Core
open! Bonsai_synth_core

val sin : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val saw : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val saw_blep : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val square : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val square_blep : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val triangle : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
val triangle_blep : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t
