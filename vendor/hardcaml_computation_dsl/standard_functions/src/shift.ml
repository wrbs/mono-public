open Core
open Hardcaml
open Hardcaml_computation_dsl
open Function

module Shift (X : sig
    val shift : Function_tag.Shift.t
  end) =
struct
  open X

  let tag =
    let open Function_tag in
    to_string (Shift shift)
  ;;

  let infer_single_output_precision (input_precisions : Precision.t list) =
    let val_precision, shift_precision =
      match input_precisions with
      | [ a; b ] -> a, b
      | _ -> failwith "invalid number of arguments for [shift]"
    in
    if shift_precision.f_bits <> 0 || shift_precision.signed
    then
      raise_s
        [%message
          "shift value is required to be an unsigned integer!"
            (shift_precision : Precision.t)];
    let max_shift = (1 lsl shift_precision.i_bits) - 1 in
    (* Extend the input precision by enough to account for any possible shift - the user
       can resize back down if they want less precision. *)
    match shift with
    | Left -> { val_precision with i_bits = val_precision.i_bits + max_shift }
    | Right | Right_arithmetic | Right_logical ->
      { val_precision with f_bits = val_precision.f_bits + max_shift }
  ;;

  let infer_output_precision = infer_single_output_precision

  let create _scope ~clocking:_ ~build_mode:_ ~input_precisions (inputs : Input.t) =
    let input_precision = List.nth_exn input_precisions 0 in
    let output_precision = infer_single_output_precision input_precisions in
    let open Signal in
    let value =
      let raw_value = List.nth_exn inputs 0 in
      match shift with
      | Left ->
        if input_precision.signed
        then sresize raw_value ~width:(Precision.width output_precision)
        else uresize raw_value ~width:(Precision.width output_precision)
      | Right | Right_arithmetic | Right_logical ->
        raw_value @: zero (output_precision.f_bits - input_precision.f_bits)
    in
    let op =
      match shift with
      | Left -> sll
      | Right -> if input_precision.signed then sra else srl
      | Right_arithmetic -> sra
      | Right_logical -> srl
    in
    let shift = List.nth_exn inputs 1 in
    { Data.q = log_shift ~f:op value ~by:shift; overflow = Signal.gnd }
  ;;
end

let node ~shift ?label a b =
  let open Shift (struct
      let shift = shift
    end) in
  Node.generic_function_single
    ?label
    ~tag
    ~latency:0
    ~infer_output_precision
    ~create
    ~propagate_input_overflows:None
    ~shared:false
    [ a; b ]
;;

let sll = node ~shift:Left
let sr = node ~shift:Right
let srl = node ~shift:Right_logical
let sra = node ~shift:Right_arithmetic
