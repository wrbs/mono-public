open! Core
open! Bonsai_synth_core
open Bonsai.Let_syntax

let two_pi = 2. *. Float.pi

module Basic_waveform = struct
  let sin x = Float.sin (x *. two_pi)
  let saw x = 1. -. (2. *. x)
  let square x = if x <=. 0.5 then 1. else -1.

  let triangle x =
    let value = -.saw x in
    2. *. (Float.abs value -. 0.5)
  ;;

  let poly_blep x ~dx =
    if x <. dx
    then (
      let y = x /. dx in
      y +. y -. (y *. y) -. 1.)
    else if x >. 1. -. dx
    then (
      let y = (x -. 1.) /. dx in
      y +. y +. (y *. y) +. 1.)
    else 0.
  ;;

  let saw_blep x ~dx = saw x -. poly_blep x ~dx
  let square_blep x ~dx = square x +. poly_blep x ~dx -. poly_blep ((x +. 0.5) %. 1.) ~dx

  let triangle_blep x ~dx ~last_output =
    let s = square_blep x ~dx in
    (dx *. s) +. ((1. -. dx) *. last_output)
  ;;
end

(* For now for simplicity, just support audio-rate frequencies *)

let make_osc ~freq ~init ~next_sample graph =
  let dx = freq *.| Sample_rate.sample_length_sec graph in
  stateful
    ~init
    (let%arr dx and next_sample in
     fun state -> Block.fold_map dx ~init:state ~f:(fun state dx -> next_sample state ~dx))
    graph
;;

let make_osc_x_dx f =
  stage (fun ~freq graph ->
    make_osc
      ~freq
      ~init:0.
      ~next_sample:
        (Bonsai.return (fun x ~dx ->
           let value = f x ~dx in
           let x' = (x +. dx) %. 1. in
           x', value))
      graph)
;;

let make_osc_x_only f = make_osc_x_dx (fun x ~dx:_ -> f x)
let sin = unstage (make_osc_x_only Basic_waveform.sin)
let saw = unstage (make_osc_x_only Basic_waveform.saw)
let saw_blep = unstage (make_osc_x_dx Basic_waveform.saw_blep)
let square = unstage (make_osc_x_only Basic_waveform.square)
let square_blep = unstage (make_osc_x_dx Basic_waveform.square_blep)
let triangle = unstage (make_osc_x_only Basic_waveform.triangle)

let triangle_blep ~freq graph =
  make_osc
    ~freq
    ~init:(~x:0., ~last_output:0.)
    ~next_sample:
      (Bonsai.return (fun (~x, ~last_output) ~dx ->
         let value = Basic_waveform.triangle_blep x ~dx ~last_output in
         let x' = (x +. dx) %. 1. in
         (~x:x', ~last_output:value), value))
    graph
;;
