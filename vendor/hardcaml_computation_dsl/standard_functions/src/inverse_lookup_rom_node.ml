open Core
open Hardcaml
module Clocking = Clocking
open Hardcaml_computation_dsl
open Function

module Make (X : sig
    val rom_latency : int
    val output_f_bits : int
  end) =
struct
  open X

  let latency = rom_latency
  let rom_round_nearest = true

  let tag =
    let open Function_tag in
    to_string
      (Fixed_op
         { op = Inverse_lookup { round_nearest = rom_round_nearest }
         ; latency = rom_latency
         })
  ;;

  let validate_and_return_input_precisions (input_precisions : Precision.t list) =
    match input_precisions with
    | [ precision ] ->
      if precision.signed
      then
        raise_s
          [%message "[Inverse_lookup_rom] only supported unsigned inputs right now."];
      precision
    | _ -> failwith "[Inverse_lookup_rom] assumes a single input"
  ;;

  let validate_input_precisions input_precisions =
    ignore (validate_and_return_input_precisions input_precisions : Precision.t)
  ;;

  let infer_output_precision input_precisions =
    let input_precision = validate_and_return_input_precisions input_precisions in
    { Precision.signed = false
    ; i_bits = Inverse_lookup_rom.output_i_bits - 1 + input_precision.f_bits
    ; f_bits = output_f_bits
    }
  ;;

  let create
    scope
    ~(clocking : _ Clocking.t)
    ~build_mode:_
    ~(input_precisions : Precision.t list)
    (inputs : Input.t)
    =
    let input_precision = validate_and_return_input_precisions input_precisions in
    assert (List.length inputs = List.length input_precisions);
    let module R =
      Inverse_lookup_rom.Make (struct
        let rom_round_nearest = rom_round_nearest
        let rom_latency = rom_latency
        let input_i_bits = input_precision.i_bits + input_precision.f_bits

        (* To achieve [output_f_bits] on the inverse, the ROM needs to account for the
           fractional bits in the input. *)
        let output_f_bits = output_f_bits + input_precision.f_bits
      end)
    in
    let%tydi { q; error } =
      R.hierarchical scope { clock = clocking.clock; a = List.nth_exn inputs 0 }
    in
    (* Because we know the output is always unsigned, drop the top sign bit from the
       inverse ROM. *)
    { Data.q = Signal.drop_top ~width:1 q; overflow = error }
  ;;
end

let node ?(shared = false) ?label ~rom_latency ~output_f_bits a =
  let module M =
    Make (struct
      let rom_latency = rom_latency
      let output_f_bits = output_f_bits
    end)
  in
  M.validate_input_precisions [ Node.precision a ];
  Node.generic_function_single
    ?label
    ~tag:M.tag
    ~latency:M.latency
    ~infer_output_precision:M.infer_output_precision
    ~create:M.create
    ~propagate_input_overflows:None
    ~shared
    [ a ]
;;
