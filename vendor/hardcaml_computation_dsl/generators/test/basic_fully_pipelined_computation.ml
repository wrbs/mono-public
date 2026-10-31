open Core
open Hardcaml_computation_dsl
module Standard_functions = Hardcaml_computation_dsl_standard_functions

module Output = struct
  let signed = { Precision.signed = true; i_bits = 20; f_bits = 8 }
  let unsigned = { Precision.signed = false; i_bits = 20; f_bits = 8 }

  type 'a t =
    { q1 : 'a [@bits Precision.width signed]
    ; q2 : 'a [@bits Precision.width signed]
    ; q3 : 'a [@bits Precision.width signed]
    ; q4 : 'a
    ; q5 : 'a
    ; q6 : 'a [@bits Precision.width unsigned]
    ; q7 : 'a [@bits Precision.width signed]
    ; q8 : 'a [@bits Precision.width signed]
    ; q9 : 'a [@bits Precision.width unsigned]
    ; q10 : 'a
    ; q11 : 'a
    ; q12 : 'a
    ; q13_valid : 'a
    ; q13_value : 'a [@bits Precision.width signed]
    ; q14 : 'a [@bits Precision.width unsigned]
    ; q15 : 'a [@bits Precision.width unsigned]
    ; q16 : 'a [@bits Precision.width signed]
    ; q17 : 'a [@bits Precision.width signed]
    ; q18 : 'a [@bits Precision.width signed]
    ; q19 : 'a [@bits Precision.width signed]
    }
  [@@deriving hardcaml, compare]

  let precision =
    { q1 = signed
    ; q2 = signed
    ; q3 = signed
    ; q4 = Precision.bool
    ; q5 = Precision.bool
    ; q6 = unsigned
    ; q7 = signed
    ; q8 = signed
    ; q9 = unsigned
    ; q10 = Precision.bool
    ; q11 = Precision.bool
    ; q12 = Precision.bool
    ; q13_valid = Precision.bool
    ; q13_value = signed
    ; q14 = unsigned
    ; q15 = unsigned
    ; q16 = signed
    ; q17 = signed
    ; q18 = signed
    ; q19 = signed
    }
  ;;
end

module Dynamic_inputs = struct
  let input_precision = { Precision.signed = true; i_bits = 5; f_bits = 1 }
  let input_width = Precision.width input_precision

  type 'a t =
    { base : 'a [@bits input_width]
    ; input0 : 'a [@bits input_width]
    ; input1 : 'a [@bits input_width]
    ; input2 : 'a [@bits input_width]
    ; input3 : 'a [@bits input_width]
    ; input6 : 'a [@bits input_width]
    }
  [@@deriving hardcaml, compare]

  let signed =
    { base = true
    ; input0 = false
    ; input1 = true
    ; input2 = false
    ; input3 = true
    ; input6 = true
    }
  ;;

  let precision = map signed ~f:(fun signed -> { input_precision with signed })
end

module Static_inputs = struct
  let p1 = { Precision.signed = true; i_bits = 5; f_bits = 1 }
  let width1 = Precision.width p1
  let p2 = { Precision.signed = false; i_bits = 3; f_bits = 0 }
  let width2 = Precision.width p2

  type 'a t =
    { scale0 : 'a [@bits width1]
    ; scale1 : 'a [@bits width1]
    ; offset1 : 'a [@bits width1]
    ; shift2 : 'a [@bits width2]
    ; shift3 : 'a [@bits width2]
    }
  [@@deriving hardcaml, compare]

  let precision =
    { scale0 = { p1 with signed = false }
    ; scale1 = { p1 with signed = true }
    ; offset1 = { p1 with signed = false }
    ; shift2 = p2
    ; shift3 = p2
    }
  ;;
end

let model
  ~(dynamic_inputs : float Dynamic_inputs.t)
  ~(static_inputs : float Static_inputs.t)
  =
  let%tydi { base; input0; input1; input2; input3; input6 } = dynamic_inputs in
  let%tydi { scale0; scale1; offset1; shift2; shift3 } = static_inputs in
  let open Float in
  let clip x = if x > 0. then x else 0. in
  let q1 = base + (scale0 * input0) + (scale1 * clip (input1 - offset1)) in
  let q2 = base + input1 in
  let q3 = base + (input2 * (2. ** shift2)) + (input3 / (2. ** shift3)) in
  let q4 = if not (input2 > input0 && scale0 < offset1) then 1. else 0. in
  let q7 = input0 - input2 in
  let q8 =
    let case = Float.to_int shift2 land 0b11 in
    let open Int in
    if case = 0
    then input1
    else if case = 1
    then input3
    else if case = 2
    then input6
    else if case = 3
    then input1
    else failwith "impossible"
  in
  let ~q:q1, ~overflow:overflow1 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q1 q1
  in
  let ~q:q2, ~overflow:overflow2 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q2 q2
  in
  let ~q:q3, ~overflow:overflow3 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q3 q3
  in
  let ~q:q5, ~overflow:overflow5 =
    if input0 < input2
    then Q_format_utils.round_to_precision ~precision:Precision.bool input0
    else (
      let ~q, ~overflow:_ =
        Q_format_utils.round_to_precision ~precision:Precision.bool input2
      in
      ~q, ~overflow:false)
  in
  let ~q:q6, ~overflow:overflow6 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q6 input6
  in
  let ~q:q7, ~overflow:overflow7 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q7 q7
  in
  let ~q:q8, ~overflow:overflow8 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q8 q8
  in
  let ~q:q9, ~overflow:overflow9 =
    if input2 = 0.
    then ~q:0., ~overflow:true
    else
      Q_format_utils.round_to_precision
        ~rounding:`Round_nearest
        ~precision:Output.precision.q9
        (1. /. input2)
  in
  (* There is no overflow for these operations because the output precision is increased
     appropriately. *)
  let q10 = if base * input1 > input0 then 1. else 0. in
  let q11 = if abs base + abs input1 = input0 then 1. else 0. in
  let q12 = if abs (base + input1) < input0 then 1. else 0. in
  let q13_valid, q13_value =
    if base > input1
    then 1., Some base
    else if input1 > input3
    then 1., Some input1
    else if input3 > input6
    then 1., Some input6
    else 0., None
  in
  let q14 = if input0 > input2 then input0 else input2 in
  (* The selection inputs cannot overflow. *)
  let q15 = (Float.to_int input0 land Int.((1 lsl 3) - 1)) |> Float.of_int in
  let q16 =
    ((Float.to_int (input1 *. 2.) asr 3) land Int.((1 lsl 2) - 1)) |> Float.of_int
  in
  (* Test mixed signedness arithmetic *)
  let q17 = base + input0 in
  let q18 = input1 - input0 in
  let q19 = base * input2 in
  let ~q:q17, ~overflow:overflow17 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q17 q17
  in
  let ~q:q18, ~overflow:overflow18 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q18 q18
  in
  let ~q:q19, ~overflow:overflow19 =
    Q_format_utils.round_to_precision ~precision:Output.precision.q19 q19
  in
  (* We only check the values of the fields that do not overflow. *)
  ( ~q:{ Output.q1 = Option.some_if (not overflow1) q1
       ; q2 = Option.some_if (not overflow2) q2
       ; q3 = Option.some_if (not overflow3) q3
       ; q4 = Some q4
       ; q5 = Option.some_if (not overflow5) q5
       ; q6 = Option.some_if (not overflow6) q6
       ; q7 = Option.some_if (not overflow7) q7
       ; q8 = Option.some_if (not overflow8) q8
       ; q9 = Option.some_if (not overflow9) q9
       ; q10 = Some q10
       ; q11 = Some q11
       ; q12 = Some q12
       ; q13_valid = Some q13_valid
       ; q13_value
       ; q14 = Some q14
       ; q15 = Some q15
       ; q16 = Some q16
       ; q17 = Option.some_if (not overflow17) q17
       ; q18 = Option.some_if (not overflow18) q18
       ; q19 = Option.some_if (not overflow19) q19
       }
  , ~overflow:(Some
                 (overflow1
                  || overflow2
                  || overflow3
                  || overflow5
                  || overflow6
                  || overflow7
                  || overflow8
                  || overflow9
                  || overflow17
                  || overflow18
                  || overflow19)) )
;;

module Graph = struct
  let name = "basic_test"

  let dynamic_inputs =
    Dynamic_inputs.map2
      Dynamic_inputs.port_names
      Dynamic_inputs.precision
      ~f:(fun label precision -> Node.input () ~label ~precision)
  ;;

  let static_inputs =
    Static_inputs.map2
      Static_inputs.port_names
      Static_inputs.precision
      ~f:(fun label precision -> Node.constant () ~label ~precision)
  ;;

  open Standard_functions

  let ( * ) a b = multiply ~shared:false a b |> pipeline ~label:"multiply_pipeline" ~n:2
  let ( - ) a b = subtract ~shared:false a b
  let ( + ) a b = add ~shared:false a b
  let ( > ) a b = gt a b
  let ( >= ) a b = gte a b
  let ( <= ) a b = lte a b
  let ( < ) a b = lt a b
  let ( == ) a b = equals a b
  let ( && ) a b = logical_and a b
  let not a = logical_not a
  let ( lsl ) a b = sll a b
  let ( asr ) a b = sra a b
  let mux2 = mux2

  let clip x =
    let precision = Node.precision x in
    if Core.not precision.signed then failwith "Clip requires a signed input.";
    let zero = Node.constant ~value:0 ~label:"zero" ~precision () in
    mux2 (x > zero) x zero
  ;;

  let abs x =
    let precision = Node.precision x in
    if Core.not precision.signed then failwith "abs requires a signed input.";
    let output_precision = { precision with signed = false } in
    let neg_x =
      Standard_functions.negate x
      |> Standard_functions.safe_resize ~precision:output_precision
    in
    let pos_x = Standard_functions.safe_resize ~precision:output_precision x in
    let zero = Node.constant ~value:0 ~label:"zero" ~precision:Precision.bool () in
    mux2 (x >= zero) pos_x neg_x
  ;;

  let inverse =
    inverse_lookup_rom ~rom_latency:2 ~output_f_bits:Output.precision.q9.f_bits
  ;;

  let output_resize =
    Output.map Output.precision ~f:(fun precision -> safe_resize ~precision)
  ;;

  let output =
    let%tydi { base; input0; input1; input2; input3; input6 } = dynamic_inputs in
    let%tydi { scale0; scale1; offset1; shift2; shift3 } = static_inputs in
    let q13 =
      priority_select
        ~label:"priority_select_q10"
        [ { valid = base > input1; value = base }
        ; { valid = input1 > input3; value = input1 }
        ; { valid = input3 > input6; value = input6 }
        ]
    in
    { Output.q1 =
        base
        + output_resize.q1 (scale0 * input0)
        + (scale1 * clip (output_resize.q1 input1 - output_resize.q1 offset1))
    ; q2 = base + input1
    ; q3 = base + output_resize.q3 (input2 lsl shift2) + (input3 asr shift3)
    ; q4 = not (input2 > input0 && scale0 < offset1)
    ; q5 =
        (* This output is set up to show the behavior of overflows through the [mux] node.
           The first branch almost always overflows, while the second branch never does. *)
        mux2
          (input0 < input2)
          (safe_resize input0 ~precision:Precision.bool)
          (unsafe_resize input2 ~precision:Precision.bool)
    ; q6 = output_resize.q6 input6
    ; q7 = input0 - input2
    ; q8 =
        (let case =
           unsafe_resize ~precision:{ signed = false; i_bits = 2; f_bits = 0 } shift2
         in
         mux case [ input1; input3; input6; input1 ])
    ; q9 = inverse input2
    ; q10 = base * input1 > input0
    ; q11 = abs base + abs input1 == input0
    ; q12 = abs (base + input1) < input0
    ; q13_valid = q13.valid
    ; q13_value = q13.value
    ; q14 =
        onehot_select
          [ { valid = input0 > input2; value = input0 }
          ; { valid = input0 <= input2; value = input2 }
          ]
    ; q15 =
        safe_resize ~precision:(Precision.unsigned_int 5) input0
        |> sel_bottom ~allow_signed:false ~width:3
    ; q16 =
        input1
        * Node.constant ~label:"two" ~value:2 ~precision:(Precision.signed_int 3) ()
        |> safe_resize ~precision:(Precision.signed_int 6)
        |> drop_bottom ~allow_signed:true ~width:3
        |> sel_bottom ~allow_signed:true ~width:2
    ; q17 = base + input0
    ; q18 = input1 - input0
    ; q19 = base * input2
    }
    |> Output.map2 output_resize ~f:(fun resize v -> resize v)
  ;;
end

include
  Hardcaml_computation_dsl_generators.Fully_pipelined.Make
    (Dynamic_inputs)
    (Static_inputs)
    (Output)
    (Graph)
