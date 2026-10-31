open Core
open Hardcaml_computation_dsl
module Standard_functions = Hardcaml_computation_dsl_standard_functions

module Inclusive_integer_range =
  Standard_functions.Hardcaml.Divide_by_power.Inclusive_integer_range

module Config = struct
  let base = 5
  let pow_base_bounds = Inclusive_integer_range.create ~min:(-3) ~max:9
  let input_i_bits = 64
  let output_i_bits = 27
  let rom_latency = 1
  let multiply_latency = 3
end

module Output = struct
  let p = { Precision.signed = true; i_bits = Config.output_i_bits; f_bits = 0 }

  type 'a t =
    { quotient : 'a [@bits Precision.width p]
    ; verify_division : 'a
    }
  [@@deriving hardcaml, compare]

  let precision = { quotient = p; verify_division = Precision.bool }
end

module Dynamic_inputs = struct
  let input_precision =
    { Precision.signed = true; i_bits = Config.input_i_bits; f_bits = 0 }
  ;;

  type 'a t =
    { pow : 'a
         [@bits Inclusive_integer_range.num_bits_to_represent Config.pow_base_bounds]
    ; dividend : 'a [@bits Precision.width input_precision]
    }
  [@@deriving hardcaml, compare]

  let precision =
    { pow = Inclusive_integer_range.precision Config.pow_base_bounds
    ; dividend = input_precision
    }
  ;;
end

module Static_inputs = struct
  include Hardcaml.Interface.Empty

  let compare _ _ _ = 0
  let precision = Empty
end

let model ~(dynamic_inputs : float Dynamic_inputs.t) ~static_inputs:_ =
  let%tydi { pow = pow_base; dividend } = dynamic_inputs in
  let open Bigint in
  let pow_base = of_float pow_base in
  let dividend = of_float dividend in
  let expect_overflow =
    ~q:Output.(map port_widths ~f:(Fn.const None)), ~overflow:(Some true)
  in
  let expect_error =
    ~q:{ Output.quotient = None; verify_division = Some zero }, ~overflow:None
  in
  let expect quotient =
    ( ~q:{ Output.quotient = Some quotient; verify_division = Some one }
    , ~overflow:(Some false) )
  in
  let detect_overflow quotient =
    let output_max = (one lsl Int.(Output.port_widths.quotient - 1)) - one in
    let output_min = -(one lsl Int.(Output.port_widths.quotient - 1)) in
    quotient < output_min || quotient > output_max
  in
  let ~q, ~overflow =
    if pow_base <= zero
    then (
      (* Multiplication. *)
      let result = dividend * pow (of_int Config.base) (-pow_base) in
      if detect_overflow result then expect_overflow else expect result)
    else (
      (* Division. *)
      let pow = pow (of_int Config.base) pow_base in
      if dividend % pow = zero
      then (
        let result = dividend / pow in
        (* Annoyingly, even though this is technically an overflow, our circuit detects it
           as an "error". *)
        if detect_overflow result then expect_error else expect result)
      else expect_error)
  in
  ~q:(Output.map q ~f:(Option.map ~f:Bigint.to_float)), ~overflow
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

  let div_by_pow5 =
    Standard_functions.divide_by_power
      ~shared:false
      ~base:Config.base
      ~pow_base_bounds:Config.pow_base_bounds
      ~output_i_bits:Config.output_i_bits
      ~rom_latency:Config.rom_latency
      ~multiply_latency:Config.multiply_latency
      ~check_for_error:false
      ()
  ;;

  let verify_div_by_pow5 =
    Standard_functions.verify_divide_by_power
      ~shared:false
      ~base:Config.base
      ~pow_base_bounds:Config.pow_base_bounds
      ~input_i_bits:Config.input_i_bits
      ~rom_latency:Config.rom_latency
      ~multiply_latency:Config.multiply_latency
      ()
  ;;

  let output =
    let%tydi { pow; dividend } = dynamic_inputs in
    let quotient = div_by_pow5 ~dividend ~pow in
    { Output.quotient; verify_division = verify_div_by_pow5 ~quotient ~pow ~dividend }
  ;;
end

include
  Hardcaml_computation_dsl_generators.Fully_pipelined.Make
    (Dynamic_inputs)
    (Static_inputs)
    (Output)
    (Graph)
