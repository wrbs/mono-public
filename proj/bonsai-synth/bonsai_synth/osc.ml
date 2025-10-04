open! Core
open! Bonsai_synth_core
open Bonsai.Let_syntax

let two_pi = 2. *. Float.pi

let sin ~freq graph =
  (* state = initial phase. By storing the phase, we allow the frequency to
     change smoothly without any jumps -- the frequency only affects the
     delta of the next sample in angle space rather than anything absolute. *)
  stateful
    ~init:0.
    (let%arr freq = freq
     and sample_length = Sample_rate.sample_length_sec graph in
     let sample_omega = sample_length *. two_pi *. freq in
     fun phase ->
       let block =
         Block.make (fun idx ->
           let omega = phase +. (Float.of_int idx *. sample_omega) in
           Float.sin omega)
       in
       let next_phase = (phase +. (sample_omega *. Float.of_int Block.size)) %. two_pi in
       next_phase, block)
    graph
;;

let sin' ~freq graph =
  (* using the above idea to actually allow pretty dynamic frequency switching *)
  stateful
    ~init:0.
    (let%arr freq = freq
     and sample_length = Sample_rate.sample_length_sec graph in
     fun init_phase ->
       let next_phase, block =
         Block.fold_map freq ~init:init_phase ~f:(fun phase freq ->
           let phase' = phase +. (sample_length *. two_pi *. freq) in
           phase', Float.sin phase)
       in
       next_phase %. two_pi, block)
    graph
;;
