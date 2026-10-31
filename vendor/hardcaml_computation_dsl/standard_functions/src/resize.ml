open Core
open Hardcaml
open Hardcaml_computation_dsl
open Function
open Function_tag.Resize

module Resize (X : sig
    val output_precision : Precision.t
    val is_safe : Is_safe.t
    val round : Round.t option
    val overflow : Overflow.t option
  end) =
struct
  open X

  let tag =
    let open Function_tag in
    Resize (is_safe, round, overflow) |> to_string
  ;;

  let infer_output_precision input_precisions =
    assert (List.length input_precisions = 1);
    output_precision
  ;;

  let create
    _scope
    ~clocking:_
    ~build_mode:_
    ~(input_precisions : Precision.t list)
    (inputs : Input.t)
    =
    let input_precision = List.hd_exn input_precisions in
    let module Output_fixed_point =
      (val if output_precision.signed
           then
             (module Hardcaml_fixed_point.Signed (Signal)
             : Hardcaml_fixed_point.Fixed_point
               with type bits = Signal.t)
           else (module Hardcaml_fixed_point.Unsigned (Signal)))
    in
    let input = List.hd_exn inputs in
    let ~fixed_input, ~casting_negative_to_unsigned =
      match input_precision.signed, output_precision.signed with
      | true, true | false, false ->
        ( ~fixed_input:(Output_fixed_point.create input_precision.f_bits input)
        , ~casting_negative_to_unsigned:Signal.gnd )
      | false, true ->
        (* When going from unsigned -> signed, we have to add one extra 0 to the msb to
           make sure the value is interpreted as signed positive. *)
        ( ~fixed_input:(Output_fixed_point.create input_precision.f_bits (Signal.ue input))
        , ~casting_negative_to_unsigned:Signal.gnd )
      | true, false ->
        (* When going from signed -> unsigned, we have to check whether the top bit is 1 -
           if so, this is an invalid conversion and it overflows. *)
        ( ~fixed_input:(Output_fixed_point.create input_precision.f_bits input)
        , ~casting_negative_to_unsigned:(Signal.msb input) )
    in
    let%tydi { value = q; valid = doesn't_overflow } =
      Output_fixed_point.resize_with_valid
        ?round:
          (Option.map round ~f:(function
            | Neg_infinity -> Output_fixed_point.Round.neg_infinity
            | Pos_infinity -> Output_fixed_point.Round.pos_infinity
            | To_zero -> Output_fixed_point.Round.to_zero
            | Away_from_zero -> Output_fixed_point.Round.away_from_zero
            | Tie_to_neg_infinity -> Output_fixed_point.Round.tie_to_neg_infinity
            | Tie_to_pos_infinity -> Output_fixed_point.Round.tie_to_pos_infinity
            | Tie_to_zero -> Output_fixed_point.Round.tie_to_zero
            | Tie_away_from_zero -> Output_fixed_point.Round.tie_away_from_zero
            | Tie_to_nearest_even -> Output_fixed_point.Round.tie_to_nearest_even
            | Tie_to_nearest_odd -> Output_fixed_point.Round.tie_to_nearest_odd))
        ?overflow:
          (Option.map overflow ~f:(function
            | Wrap -> Output_fixed_point.Overflow.wrap
            | Saturate -> Output_fixed_point.Overflow.saturate))
        fixed_input
        output_precision.i_bits
        output_precision.f_bits
    in
    let ( |: ) = Signal.( |: ) in
    let ( ~: ) = Signal.( ~: ) in
    let check_overflow_for_safety = ~:doesn't_overflow |: casting_negative_to_unsigned in
    { Data.q = Output_fixed_point.signal q
    ; overflow =
        (match is_safe with
         | Unsafe -> Signal.gnd
         | Safe -> check_overflow_for_safety
         | Lossless ->
           check_overflow_for_safety
           |: ~:(Output_fixed_point.round_is_lossless fixed_input output_precision.f_bits))
    }
  ;;
end

type t =
  ?label:string
  -> ?round:Round.t
  -> ?overflow:Overflow.t
  -> precision:Precision.t
  -> Node.t
  -> Node.t

module Node (X : sig
    val is_safe : Is_safe.t
  end) =
struct
  let node ?label ?round ?overflow ~precision a =
    let open Resize (struct
        include X

        let output_precision = precision
        let round = round
        let overflow = overflow
      end) in
    Node.generic_function_single
      ?label
      ~tag
      ~latency:0
      ~infer_output_precision
      ~create
      ~propagate_input_overflows:None
      ~shared:false
      [ a ]
  ;;
end

module Lossless = Node (struct
    let is_safe = Is_safe.Lossless
  end)

module Safe = Node (struct
    let is_safe = Is_safe.Safe
  end)

module Unsafe = Node (struct
    let is_safe = Is_safe.Unsafe
  end)

let lossless = Lossless.node
let safe = Safe.node
let unsafe = Unsafe.node
