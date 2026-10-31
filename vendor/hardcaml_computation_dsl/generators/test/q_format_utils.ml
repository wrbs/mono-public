open Core
open Hardcaml_computation_dsl

let generate { Precision.signed; i_bits; f_bits } =
  let open Quickcheck.Generator.Let_syntax in
  (* Just generate a regular 2's complement integer. *)
  let%bind unscaled =
    (* We have to limit the range of our output here because [Hardcaml_fixed_point.create]
       uses [Float.to_int] internally, rather than [Bigint]. *)
    let shift = min 61 (i_bits + f_bits - 1) in
    let open Bigint in
    let max = one lsl shift in
    if signed then gen_incl (-max) (max - one) else gen_incl zero ((of_int 2 * max) - one)
  in
  (* Perform the scaling afterwards and cast to float. *)
  return (Float.( / ) (Bigint.to_float unscaled) (Float.of_int (1 lsl f_bits)))
;;

let round_to_precision ?(rounding = `Truncate) ~(precision : Precision.t) f =
  let round f =
    match rounding with
    | `Round_nearest -> Float.round_nearest f
    | `Truncate -> f
  in
  let unscaled =
    1 lsl precision.f_bits |> Float.of_int |> Float.( * ) f |> round |> Float.to_int
  in
  let width = Precision.width precision in
  let overflow =
    let max = 1 lsl (width - 1) in
    if precision.signed
    then unscaled > max - 1 || unscaled < -max
    else unscaled > (2 * max) - 1 || unscaled < 0
  in
  let truncated =
    let bits = unscaled land ((1 lsl width) - 1) in
    let top_bit = 1 lsl (width - 1) in
    if precision.signed then bits - (2 * (bits land top_bit)) else bits
  in
  let q = Float.(of_int truncated / of_int (1 lsl precision.f_bits)) in
  ~q, ~overflow
;;
