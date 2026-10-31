open Core
module Precision = Hardcaml_computation_dsl.Precision

(* Fixed-point quantization for inputs: uses round-to-nearest to match driver behavior *)
let quantize_input ~f_bits ~total_bits (f : float) : float =
  let s = Float.of_int (1 lsl f_bits) in
  let scaled = Float.round_nearest (f *. s) in
  (* Clamp to signed range *)
  let max_val = Float.of_int ((1 lsl (total_bits - 1)) - 1) in
  let min_val = Float.of_int (-(1 lsl (total_bits - 1))) in
  let clamped = Float.clamp_exn scaled ~min:min_val ~max:max_val in
  clamped /. s
;;

(* Fixed-point quantization for output: uses floor (round toward -∞) to match hardware's
   default rounding mode in safe_resize *)
let quantize_output ~f_bits ~total_bits (f : float) : float =
  let s = Float.of_int (1 lsl f_bits) in
  let scaled = Float.round_down (f *. s) in
  (* Clamp to signed range *)
  let max_val = Float.of_int ((1 lsl (total_bits - 1)) - 1) in
  let min_val = Float.of_int (-(1 lsl (total_bits - 1))) in
  let clamped = Float.clamp_exn scaled ~min:min_val ~max:max_val in
  clamped /. s
;;

let conv1d (precision : Precision.t) ~kernel ~input =
  let f_bits = precision.f_bits in
  let total_bits = Precision.width precision in
  let window_size = List.length kernel in
  let kernel = Array.of_list (List.map kernel ~f:(quantize_input ~f_bits ~total_bits)) in
  let shift_reg = Array.create ~len:window_size 0.0 in
  List.map input ~f:(fun v ->
    let input_quantized = quantize_input ~f_bits ~total_bits v in
    (* Shift elements FIRST: oldest drops out, everything shifts down, new input goes to
       end. The hardware pipelines the start signal by 1 cycle, so the computation sees
       the shift register state AFTER the new input has been shifted in. *)
    for i = 0 to window_size - 2 do
      shift_reg.(i) <- shift_reg.(i + 1)
    done;
    shift_reg.(window_size - 1) <- input_quantized;
    (* THEN compute dot product using the updated shift register state *)
    let sum = ref 0.0 in
    for i = 0 to window_size - 1 do
      sum := !sum +. (shift_reg.(i) *. kernel.(i))
    done;
    (* Quantize output back to element precision using floor rounding to match hardware's
       safe_resize default behavior *)
    quantize_output ~f_bits ~total_bits !sum)
;;
