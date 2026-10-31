open Core
open Hardcaml_computation_dsl

(* We want to find the quotient and remainder after dividing [dividend] by a constant
   [divisor]. Let the bit-width of [dividend] be some maximum value [dividend_bits].

   To do this, we first factor out the largest power of 2 we can from [divisor], let this
   be 2^N, and let [divisor = 2^N * divisor'], where [divisor'] is odd.

   If we divide dividend by 2^N, we can write dividend = 2^N * Q1 + R1, where R1 ∈ [0, 2^N).

   Then, if we divide Q1 by divisor', we can write Q1 = divisor' * Q2 + R2, where R2 ∈ [0,
   divisor').

   Combining these, dividend = divisor * Q2 + R3, where R3 = (2^N * R2 + R1) ∈ [0, divisor).

   Q1, R1 are easy to find. So, the only difficulty is in finding R2 - we can do this with
   a large multiplication to find Q2, and then subtraction to find R2.

   Specifically, to compute Q2, we use the following idea:

   Q2 = floor(Q1 / divisor')
      = floor((Q1 / divisor') * (2^k / 2^k))
      = floor(Q1 * (2^k / divisor') * (1 / 2^k))

   The idea is that, if we choose a large enough k, we can approximate the middle term,
   and do the final division as a truncation, and the result will still be correct.

   To decide how large k needs to be, we note the following:

   Let m' = (2^k / divisor') and approximate it by m = ceil(m'). Then, note that m = m' +
   e, where e is an error term with e ∈ [0, 1).

   Then, the expression that we will compute is

   floor((Q1 * m) / 2^k)
   = floor((Q1 * (m' + e)) / 2^k)
   = floor(Q1 * m' / 2^k + Q1 * e / 2^k)
   = floor(Q1 / divisor' + (Q1 * e / 2^k))

   Comparing this to the original expression for Q2 (floor(Q1 / divisor')), we see that in
   order for the approximation to be equivalent to the actual value, it is sufficient for

   (Q1 * e / 2^k) < 1 / divisor'
   <=> k > log2(Q1 * e * divisor')

   Noting that Q1 < (2 ^ (dividend_bits - N)) and e < 1, we can choose
   k > (dividend_bits - N) + log2(divisor').

   For signed dividends, we do the same computation but do the floor((Q1 * m) / 2^k) just
   using the positive Q1 value and then convert back to a negative Q2 if necessary. When
   negating, we use a bitwise not instead of a typical negation, as this results in the
   correct rounding behavior and conveniently is a much simpler circuit with no need to
   worry about overflow when dealing with the most negative value 0b10000....
*)

module Untyped = struct
  module Coefficients = struct
    type 'a t =
      { n : 'a
      ; divisor' : 'a
      ; m : 'a
      ; k : 'a
      }
    [@@deriving fields ~getters]

    (* In the prime factorization of n, how many times does p appear *)
    let p_adic_valuation ~p n =
      let mutable r = 0 in
      let mutable n = n in
      while n % p = 0 do
        r <- r + 1;
        n <- n / p
      done;
      r
    ;;

    let create ~unsigned_dividend_bits ~divisor =
      assert (divisor > 0);
      if Bigint.(of_int divisor >= one lsl unsigned_dividend_bits)
      then
        raise_s
          [%message
            "Divisor is larger than largest possible dividend"
              (unsigned_dividend_bits : int)
              (divisor : int)];
      let n = p_adic_valuation ~p:2 divisor in
      let divisor' = divisor lsr n in
      let k = unsigned_dividend_bits - n + Int.ceil_log2 divisor' in
      let m =
        let floor = Bigint.((one lsl k) / of_int divisor' |> to_int_exn) in
        if divisor' = 1 then floor else floor + 1
      in
      { n; divisor'; k; m }
    ;;
  end
end

module Make (Config : sig
    val dividend_signed : bool
    val dividend_bits : int
    val divisors : int list
  end) =
struct
  open Config

  let unsigned_dividend_bits =
    if dividend_signed then dividend_bits - 1 else dividend_bits
  ;;

  let min_divisor = List.min_elt divisors ~compare:Int.compare |> Option.value_exn
  let max_divisor = List.max_elt divisors ~compare:Int.compare |> Option.value_exn

  let coefficients_lookup =
    List.map divisors ~f:(fun divisor ->
      divisor, Untyped.Coefficients.create ~unsigned_dividend_bits ~divisor)
    |> Int.Map.of_alist_exn
  ;;

  let get_coefficients ~divisor = Map.find coefficients_lookup divisor
  let n_values = Map.data coefficients_lookup |> List.map ~f:Untyped.Coefficients.n

  let max_coefficients_n_value =
    List.max_elt n_values ~compare:Int.compare |> Option.value_exn
  ;;

  let min_coefficients_n_value =
    List.min_elt n_values ~compare:Int.compare |> Option.value_exn
  ;;

  let max_coefficients_divisor'_value =
    Map.data coefficients_lookup
    |> List.map ~f:Untyped.Coefficients.divisor'
    |> List.max_elt ~compare:Int.compare
    |> Option.value_exn
  ;;

  let make_nonzero i =
    assert (i >= 0);
    if i = 0 then 1 else i
  ;;

  let unsigned_q1_bits = make_nonzero (unsigned_dividend_bits - min_coefficients_n_value)
  let q1_bits = make_nonzero (dividend_bits - min_coefficients_n_value)
  let r1_bits = make_nonzero max_coefficients_n_value

  let min_unsigned_quotient_bits ~divisor =
    let max_dividend = Bigint.((one lsl unsigned_dividend_bits) - one) in
    Bigint.(max_dividend / of_int divisor |> to_int_exn)
    |> Hardcaml.Signal.num_bits_to_represent
  ;;

  let min_q2_bits =
    let min_unsigned_q2_bits = min_unsigned_quotient_bits ~divisor:max_divisor in
    if dividend_signed then min_unsigned_q2_bits + 1 else min_unsigned_q2_bits
  ;;

  let unsigned_q2_bits = min_unsigned_quotient_bits ~divisor:min_divisor
  let q2_bits = if dividend_signed then unsigned_q2_bits + 1 else unsigned_q2_bits
  let r2_bits = make_nonzero (Int.ceil_log2 max_coefficients_divisor'_value)
  let r3_bits = make_nonzero (Int.ceil_log2 max_divisor)

  module Coefficients = struct
    module T = struct
      let bits_for getter =
        Map.data coefficients_lookup
        |> List.map ~f:getter
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
        |> Hardcaml.Signal.num_bits_to_represent
        |> make_nonzero
      ;;

      type 'a t = 'a Untyped.Coefficients.t =
        { n : 'a [@bits bits_for Untyped.Coefficients.n]
        ; divisor' : 'a
             [@bits bits_for Untyped.Coefficients.divisor'] [@rtlname "divisor_"]
        ; m : 'a [@bits bits_for Untyped.Coefficients.m]
        ; k : 'a [@bits bits_for Untyped.Coefficients.k]
        }
      [@@deriving hardcaml]

      let precision = map port_widths ~f:Precision.unsigned_int
    end

    include T

    module With_divisor = struct
      let name = "coefficients_with_divisor"

      type 'a t =
        { divisor : 'a [@bits Hardcaml.Signal.num_bits_to_represent max_divisor]
        ; coefficients : 'a T.t
        }
      [@@deriving hardcaml]

      let precision =
        { divisor = Precision.unsigned_int port_widths.divisor
        ; coefficients = T.precision
        }
      ;;

      include functor Hardcaml.With_valid.Wrap.Include.Make
      include functor Hardcaml_axi.Register_bank.Packed_array.Include.Make
    end
  end

  module Dsl_utils = struct
    include Hardcaml_computation_dsl_standard_functions

    let ( - ) a b = subtract a b
    let pipeline = pipeline
  end

  let verify_unsigned_width x ~width =
    if Bigint.(x asr width <> zero)
    then raise_s [%message "Width check failed" (x : Bigint.t) (width : int)]
  ;;

  module Compute_q1_and_r1 = struct
    module I = struct
      type 'a t =
        { dividend : 'a [@bits dividend_bits]
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      let precision =
        { dividend =
            { Precision.signed = dividend_signed; i_bits = dividend_bits; f_bits = 0 }
        ; coefficients = Coefficients.precision
        }
      ;;
    end

    module O = struct
      type 'a t =
        { unsigned_q1 : 'a [@bits unsigned_q1_bits]
        ; q1 : 'a [@bits q1_bits]
        ; r1 : 'a [@bits r1_bits]
        ; dividend_msb : 'a option [@exists dividend_signed]
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      let precision =
        { (map port_widths ~f:Precision.unsigned_int) with
          q1 = { Precision.signed = dividend_signed; i_bits = q1_bits; f_bits = 0 }
        }
      ;;
    end

    let model { I.dividend; coefficients } =
      let dividend_msb =
        if dividend_signed
        then Some Bigint.(if dividend < zero then one else zero)
        else None
      in
      (* See the comment at the top of the file regarding bitwise not. *)
      let unsigned_dividend =
        if dividend_signed && Bigint.(dividend < zero)
        then Bigint.lnot dividend
        else dividend
      in
      assert (
        Bigint.(
          zero <= unsigned_dividend && unsigned_dividend < one lsl unsigned_dividend_bits));
      let unsigned_q1 = Bigint.(unsigned_dividend asr to_int_exn coefficients.n) in
      let q1 = Bigint.(dividend asr to_int_exn coefficients.n) in
      let r1 = Bigint.(dividend land ((one lsl to_int_exn coefficients.n) - one)) in
      verify_unsigned_width unsigned_q1 ~width:unsigned_q1_bits;
      verify_unsigned_width r1 ~width:r1_bits;
      { O.unsigned_q1; q1; r1; dividend_msb; coefficients }
    ;;

    let dsl { I.dividend; coefficients } =
      let dividend_precision = Node.precision dividend in
      assert (Precision.equal I.precision.dividend dividend_precision);
      let open Dsl_utils in
      let dividend_msb =
        if dividend_signed
        then
          Some
            Core.(
              select
                ~allow_signed:true
                ~high:(dividend_bits - 1)
                ~low:(dividend_bits - 1)
                dividend
              |> unsafe_resize ~precision:Precision.bool)
        else None
      in
      (* See the comment at the top of the file regarding bitwise not. *)
      let unsigned_dividend =
        match dividend_msb with
        | Some msb ->
          mux2 msb (bitwise_not ~allow_signed:true dividend) dividend
          |> unsafe_resize ~precision:(Precision.unsigned_int unsigned_dividend_bits)
        | None -> dividend
      in
      let unsigned_q1 =
        srl unsigned_dividend coefficients.n
        |> unsafe_resize ~precision:O.precision.unsigned_q1
      in
      let q1 =
        let shift_right = if dividend_signed then sra else srl in
        shift_right dividend coefficients.n |> unsafe_resize ~precision:O.precision.q1
      in
      let r1_mask =
        Node.constant
          ~label:"ones"
          ~precision:O.precision.r1
          ~value:Core.((1 lsl r1_bits) - 1)
          ()
        |> (fun c -> sll c coefficients.n)
        |> sel_bottom ~width:r1_bits
        |> bitwise_not
      in
      let r1 =
        sel_bottom ~allow_signed:true dividend ~width:r1_bits
        |> unsafe_resize ~precision:O.precision.r1
        |> bitwise_and r1_mask
      in
      { O.unsigned_q1; q1; r1; dividend_msb; coefficients }
    ;;
  end

  module Compute_q2 = struct
    module I = Compute_q1_and_r1.O

    module O = struct
      type 'a t =
        { q1 : 'a [@bits q1_bits]
        ; r1 : 'a [@bits r1_bits]
        ; q2 : 'a [@bits q2_bits]
        ; coefficients : 'a Coefficients.t
        }
      [@@deriving hardcaml]

      let precision =
        { (map port_widths ~f:Precision.unsigned_int) with
          q1 = { Precision.signed = dividend_signed; i_bits = q1_bits; f_bits = 0 }
        ; q2 = { Precision.signed = dividend_signed; i_bits = q2_bits; f_bits = 0 }
        }
      ;;
    end

    let model { I.unsigned_q1; q1; r1; dividend_msb; coefficients } =
      let unsigned_q2 =
        Bigint.((unsigned_q1 * coefficients.m) asr to_int_exn coefficients.k)
      in
      verify_unsigned_width unsigned_q2 ~width:unsigned_q2_bits;
      (* See the comment at the top of the file regarding bitwise not. *)
      let q2 =
        match dividend_msb with
        | Some msb -> if Bigint.(msb = one) then Bigint.lnot unsigned_q2 else unsigned_q2
        | None -> unsigned_q2
      in
      { O.q1; r1; q2; coefficients }
    ;;

    let dsl
      ~division_multiply_cycles
      { I.unsigned_q1; q1; r1; dividend_msb; coefficients }
      =
      let unsigned_q2_precision = Precision.unsigned_int unsigned_q2_bits in
      let open Dsl_utils in
      let unsigned_q2 =
        let width1 = Node.precision unsigned_q1 |> Precision.width in
        let width2 = Node.precision coefficients.m |> Precision.width in
        let label =
          [%string "unsigned_q2_division_multiply__%{width1#Int}x%{width2#Int}"]
        in
        srl
          (pipeline
             ~n:division_multiply_cycles
             ~label
             (multiply ~label unsigned_q1 coefficients.m))
          coefficients.k
        |> unsafe_resize ~precision:unsigned_q2_precision
      in
      (* See the comment at the top of the file regarding bitwise not. *)
      let q2 =
        match dividend_msb with
        | Some msb ->
          let signed_q2 =
            unsafe_resize unsigned_q2 ~precision:(Precision.signed_int q2_bits)
          in
          mux2 msb (bitwise_not signed_q2 ~allow_signed:true) signed_q2
        | None -> unsigned_q2
      in
      { O.q1; r1; q2; coefficients }
    ;;
  end

  module Compute_r3 = struct
    module I = Compute_q2.O

    module O = struct
      type 'a t =
        { q2 : 'a [@bits q2_bits]
        ; r3 : 'a [@bits r3_bits]
        }
      [@@deriving hardcaml]

      let precision =
        { (map port_widths ~f:Precision.unsigned_int) with
          q2 = { Precision.signed = dividend_signed; i_bits = q2_bits; f_bits = 0 }
        }
      ;;
    end

    let model { I.q1; r1; q2; coefficients } =
      let r2 = Bigint.(q1 - (q2 * coefficients.divisor')) in
      let r3 = Bigint.((r2 lsl to_int_exn coefficients.n) lor r1) in
      verify_unsigned_width r2 ~width:r2_bits;
      verify_unsigned_width r3 ~width:r3_bits;
      { O.q2; r3 }
    ;;

    let dsl
      ~remainder_multiply_cycles
      ~remainder_subtraction_cycles
      { I.q1; r1; q2; coefficients }
      =
      let r2_precision = Precision.unsigned_int r2_bits in
      let open Dsl_utils in
      let q2_scaled_by_divisor' =
        let label = "div_by_const_remainder_multiply" in
        pipeline
          ~label
          ~n:remainder_multiply_cycles
          (multiply ~label q2 coefficients.divisor')
      in
      let r2 = q1 - q2_scaled_by_divisor' |> unsafe_resize ~precision:r2_precision in
      let r3 =
        bitwise_or
          (sll r2 coefficients.n |> unsafe_resize ~precision:O.precision.r3)
          (unsafe_resize ~precision:O.precision.r3 r1)
      in
      { O.q2; r3 }
      |> O.(
           map2 port_names ~f:(fun name v ->
             pipeline
               ~label:("div_by_const_remainder_subtraction" ^ name)
               ~n:remainder_subtraction_cycles
               v))
    ;;
  end

  let model inputs =
    Compute_q1_and_r1.model inputs |> Compute_q2.model |> Compute_r3.model
  ;;

  let dsl
    ~division_multiply_cycles
    ~remainder_multiply_cycles
    ~remainder_subtraction_cycles
    inputs
    =
    Compute_q1_and_r1.dsl inputs
    |> Compute_q2.dsl ~division_multiply_cycles
    |> Compute_r3.dsl ~remainder_multiply_cycles ~remainder_subtraction_cycles
  ;;

  module For_testing = struct
    let expect ~dividend ~divisor =
      { Compute_r3.O.q2 = Bigint.(dividend /% divisor); r3 = Bigint.(dividend % divisor) }
    ;;
  end
end
