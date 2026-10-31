open! Base
open Core
open Hardcaml
open Signal
module Fixed_signed = Hardcaml_fixed_point.Signed (Signal)

module Make (Config : sig
    val i_bits : int
    val f_bits : int
  end) =
struct
  include Config

  module With_overflow = struct
    type t =
      { value : Fixed_signed.t
      ; overflow : Signal.t
      }
  end

  module Fixed_signed_as_float = struct
    type nonrec t = Fixed_signed.t

    let sexp_of_t t = Float.sexp_of_t (Fixed_signed.to_float t)
  end

  let of_float = Fixed_signed.of_float i_bits f_bits
  let round = Fixed_signed.Round.tie_away_from_zero
  let overflow = Fixed_signed.Overflow.wrap
  let fp i = Fixed_signed.(create f_bits (Signal.sresize i ~width:(f_bits + i_bits)))
  let fp_zero = Fixed_signed.of_float i_bits f_bits 0.
  let fp_one = Fixed_signed.of_float i_bits f_bits 1.
  let fp_neg_one = Fixed_signed.of_float i_bits f_bits (-1.)
  let fp_half = Fixed_signed.of_float i_bits f_bits 0.5
  let fp_neg_half = Fixed_signed.of_float i_bits f_bits (-0.5)

  (* Maximum value we can represent. Minus 1 as we are using signed numbers. *)
  let max_value = Fixed_signed.create f_bits (gnd @: ones (f_bits + i_bits - 1))

  (* Minimum value we can represent. *)
  let min_value = Fixed_signed.create f_bits (vdd @: zero (f_bits + i_bits - 1))

  let unsafe_resize ?(round = round) i =
    Fixed_signed.resize ~round ~overflow i i_bits f_bits
  ;;

  let safe_resize ?(round = round) i =
    let resized = Fixed_signed.resize_with_valid ~round ~overflow i i_bits f_bits in
    { With_overflow.value = resized.value; overflow = ~:(resized.valid) }
  ;;

  let fp_div_by_2 fp =
    let open Fixed_signed in
    let fp = scale_pow2 fp (-1) in
    resize ~round ~overflow fp i_bits f_bits
  ;;

  (* We check for overflow or wrap by making sure the extra bits added due to this
     operation (which will be reduced in the call to pipeline_result) are the same as the
     sign bit. *)
  let pipeline_result_and_check_overflow ~n spec d : With_overflow.t =
    let prev_f_bits = width (Fixed_signed.frac d) in
    let prev_i_bits = width (Fixed_signed.int d) in
    let d = pipeline spec ~enable:vdd ~n (Fixed_signed.signal d) in
    let d = Fixed_signed.create prev_f_bits d in
    (* We need to add 1 here to make sure we include the sign bit. *)
    let top_bits = sel_top (Fixed_signed.int d) ~width:(1 + prev_i_bits - i_bits) in
    let overflow_detected =
      ~:(mux2 (msb top_bits) (top_bits ==+. -1) (top_bits ==:. 0))
    in
    let value = Fixed_signed.(resize ~round ~overflow d i_bits f_bits) in
    { value; overflow = overflow_detected }
  ;;

  let reduce_and_check_overflow ~n spec l op : With_overflow.t =
    let l = Fixed_signed.norm l in
    let result = reduce ~f:op l in
    pipeline_result_and_check_overflow ~n spec result
  ;;

  let multiply ~n spec l : With_overflow.t =
    reduce_and_check_overflow ~n spec l Fixed_signed.( *: )
  ;;

  let add ~n spec l : With_overflow.t =
    reduce_and_check_overflow ~n spec l Fixed_signed.( +: )
  ;;

  let sub ~n spec l : With_overflow.t =
    reduce_and_check_overflow ~n spec l Fixed_signed.( -: )
  ;;

  module For_simulation = struct
    let fp_bits_to_float bits =
      let s = Signal.of_string (Bits.to_string bits) in
      Fixed_signed.(to_float (create f_bits s))
    ;;

    let large_fp_bits_to_float bits =
      let bits = Bits.to_bigint ~signedness:Signed bits in
      let scale = Bigint.(one lsl f_bits) in
      let fixed =
        Zarith.Q.make (Bigint.to_zarith_bigint bits) (Bigint.to_zarith_bigint scale)
      in
      Zarith.Q.to_float fixed
    ;;

    let print_fp_bits bits =
      let f = fp_bits_to_float bits in
      printf "%+.7f\n" f
    ;;

    let float_to_fp_bits f =
      let s = Signal.to_bstr Fixed_signed.(signal (of_float i_bits f_bits f)) in
      Bits.of_string s
    ;;

    let bits_to_fp bits =
      let s = Signal.of_string (Bits.to_string bits) in
      Fixed_signed.(create f_bits s)
    ;;
  end
end
