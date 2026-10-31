open Core
open Hardcaml
open Signal

module Inclusive_integer_range = struct
  include Hardcaml_circuits.Exactly_divisible_by_constant_power.Inclusive_integer_range

  let within { min; max } v = v >= min && v <= max
  let ( ~- ) { min; max } = create ~min:(-max) ~max:(-min)
  let ( + ) x y = create ~min:(x.min + y.min) ~max:(x.max + y.max)
  let ( - ) x y = x + -y

  let precision t =
    { Hardcaml_computation_dsl.Precision.i_bits = num_bits_to_represent t
    ; f_bits = 0
    ; signed = need_sign_bit t
    }
  ;;
end

module type Config = sig
  val base : int
  val pow_base_bounds : Inclusive_integer_range.t
  val input_i_bits : int
  val output_i_bits : int
  val rom_latency : int
  val multiply_latency : int
  val check_for_error : bool
  val signedness : Signedness.t
end

module Make (Config : Config) = struct
  module Divider = Hardcaml_circuits.Exactly_divisible_by_constant_power.Make (Signal)

  let latency = Config.multiply_latency + Config.rom_latency
  let pow_bits = Divider.pow_bits ~bounds:Config.pow_base_bounds

  module I = struct
    type 'a t =
      { clock : 'a
      ; dividend : 'a [@bits Config.input_i_bits]
      ; pow : 'a [@bits pow_bits]
      ; valid : 'a [@rtlprefix "i_"]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { quotient : 'a [@bits Config.output_i_bits]
      ; error : 'a
      ; division_output_not_multiple : 'a
      ; valid : 'a [@rtlprefix "o_"]
      }
    [@@deriving hardcaml]
  end

  let create scope ~build_mode:_ ({ clock; dividend; pow; valid } : _ I.t) =
    let spec = Reg_spec.create ~clock () in
    let ( -- ) = Scope.naming scope in
    let pipeline_and_name prefix n name signal =
      pipeline spec ~n signal -- (prefix ^ name)
    in
    let t =
      Divider.Inverse_rom.create
        ~check_for_error:Config.check_for_error
        ~signedness:Config.signedness
        ~bounds:Config.pow_base_bounds
        ~base:Config.base
        ~output_bits:Config.output_i_bits
        ~dividend
        ~pow
      |> Divider.Inverse_rom.map ~f:(pipeline_and_name "inverse$" Config.rom_latency)
      |> Divider.Multiplication.create
      |> Divider.Multiplication.map
           ~f:(pipeline_and_name "multiply$" Config.multiply_latency)
      |> Divider.Error_check.create
      |> Divider.Error_check.map ~f:(pipeline_and_name "error$" 0)
    in
    { O.quotient = Divider.Error_check.quotient t
    ; error = Divider.Error_check.error t
    ; division_output_not_multiple = Divider.Error_check.division_output_not_multiple t
    ; valid = pipeline spec valid ~n:latency
    }
  ;;

  let hierarchical ?instance scope ~(build_mode : Build_mode.t) i =
    let module Scoped = Hierarchy.In_scope (I) (O) in
    Scoped.hierarchical ~scope ~name:"divide_by_power" ?instance (create ~build_mode) i
  ;;
end
