open! Core
open! Bonsai_synth_core

(** A sin wave with control-rate frequency [freq]. *)
val sin : freq:float Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t

(** A sin wave with audio-rate frequency [freq]. *)
val sin' : freq:Block.t Bonsai.t -> local_ Bonsai.graph -> Block.t Bonsai.t

(* TODO: saw, square, triangle, noise(/think about randomness in general) *)
(* TODO: PolyBLEP? *)
(* TODO: PWM for square? *)
