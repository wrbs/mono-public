open Core
open Hardcaml
open Signal

module type Config = sig
  val rom_round_nearest : bool
  val rom_latency : int
  val input_i_bits : int
  val output_f_bits : int
end

let output_i_bits = 2

module Make (Config : Config) = struct
  module Fixed_signed = Hardcaml_fixed_point.Signed (Signal)

  module Fixed_utils_rom_config = struct
    let i_bits = output_i_bits
    let f_bits = Config.output_f_bits
  end

  module Fixed_utils_rom = Fixed_prims.Make (Fixed_utils_rom_config)

  module I = struct
    type 'a t =
      { clock : 'a
      ; a : 'a [@bits Config.input_i_bits]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { q : 'a [@bits Config.output_f_bits + output_i_bits]
      ; error : 'a
      }
    [@@deriving hardcaml]
  end

  let divider i =
    let open Fixed_signed in
    let of_float =
      if Config.rom_round_nearest then of_float_round_nearest else of_float
    in
    let div =
      if i = 0
      then Fixed_utils_rom.fp_zero
      else of_float output_i_bits Config.output_f_bits Float.(1. / of_int i)
    in
    let div = signal div in
    (* ensure the result has reduced to a constant in order to build a ROM *)
    assert (Signal.Type.is_const div);
    div
  ;;

  let create _scope (i : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:i.clock () in
    let pipe d = pipeline spec ~n:Config.rom_latency d in
    let rom =
      Array.init (1 lsl Config.input_i_bits) ~f:(fun i ->
        divider i |> Signal.to_constant |> Bits.of_constant)
    in
    { O.q = (Signal.rom ~read_addresses:[| i.a |] rom).(0) |> pipe
    ; error = i.a ==:. 0 |> pipe
    }
  ;;

  let hierarchical scope =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~here:[%here] ~scope create
  ;;
end
