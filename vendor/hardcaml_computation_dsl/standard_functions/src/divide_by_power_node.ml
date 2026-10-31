open Core
open Hardcaml
module Clocking = Clocking
open Hardcaml_computation_dsl
open Function
module Inclusive_integer_range = Divide_by_power.Inclusive_integer_range

let tag =
  let open Function_tag in
  to_string Divide_by_power
;;

module Make (X : sig
    val base : int
    val pow_base_bounds : Inclusive_integer_range.t
    val output_i_bits : int
    val rom_latency : int
    val multiply_latency : int
    val check_for_error : bool
  end) =
struct
  (* This is an integer operation - validate that the inputs are all integers. *)
  let validate_input_precisions (input_precisions : Precision.t list) =
    assert (List.length input_precisions = 2);
    List.iter input_precisions ~f:(fun { f_bits; _ } ->
      if f_bits <> 0
      then raise_s [%message "divide_by_power only supports integer inputs!"])
  ;;

  let infer_output_precision input_precisions =
    validate_input_precisions input_precisions;
    { Precision.i_bits = X.output_i_bits
    ; f_bits = 0
    ; signed = (List.hd_exn input_precisions).signed
    }
  ;;

  let divide
    scope
    ~(clocking : _ Clocking.t)
    ~build_mode
    ~(input_precisions : Precision.t list)
    (inputs : Input.t)
    =
    validate_input_precisions input_precisions;
    assert (List.length inputs = List.length input_precisions);
    let open Signal in
    let module D =
      Divide_by_power.Make (struct
        include X

        let input_i_bits = (List.nth_exn input_precisions 0).i_bits

        let signedness =
          if (List.hd_exn input_precisions).signed then Signedness.Signed else Unsigned
        ;;
      end)
    in
    let%tydi { quotient; error; division_output_not_multiple; valid = _ } =
      D.hierarchical
        scope
        ~build_mode
        { clock = clocking.clock
        ; dividend = List.nth_exn inputs 0
        ; pow = List.nth_exn inputs 1
        ; valid = gnd (* Valid doesn't matter; we're using this fully pipelined. *)
        }
    in
    ~quotient, ~error, ~division_output_not_multiple
  ;;

  let create
    scope
    ~(clocking : _ Clocking.t)
    ~build_mode
    ~(input_precisions : Precision.t list)
    (inputs : Input.t)
    =
    let ~quotient, ~error, ~division_output_not_multiple:_ =
      divide scope ~clocking ~build_mode ~input_precisions inputs
    in
    { Data.q = quotient; overflow = error }
  ;;

  let create_is_divisible_only
    scope
    ~(clocking : _ Clocking.t)
    ~build_mode
    ~(input_precisions : Precision.t list)
    (inputs : Input.t)
    =
    if not X.check_for_error
    then
      raise_s
        [%message
          "Error checking must be enabled for [create_is_divisible_only]"
            (X.check_for_error : bool)];
    if X.pow_base_bounds.min < 0
    then
      raise_s
        [%message
          "[create_is_divisible_only] is expecting to only do division, but config with \
           negative min power allows for multiplication!"
            (X.pow_base_bounds : Inclusive_integer_range.t)];
    let ~quotient:_, ~error:_, ~division_output_not_multiple =
      divide scope ~clocking ~build_mode ~input_precisions inputs
    in
    { Data.q = Signal.( ~: ) division_output_not_multiple
    ; overflow =
        Signal.gnd (* The division is sized such that overflow is not possible. *)
    }
  ;;
end

let node
  ?(shared = false)
  ?label
  ~base
  ~pow_base_bounds
  ~output_i_bits
  ~rom_latency
  ~multiply_latency
  ~check_for_error
  ~dividend
  ~pow
  ()
  =
  let module M =
    Make (struct
      let base = base
      let pow_base_bounds = pow_base_bounds
      let output_i_bits = output_i_bits
      let rom_latency = rom_latency
      let multiply_latency = multiply_latency
      let check_for_error = check_for_error
    end)
  in
  Node.generic_function_single
    ?label
    ~tag
    ~latency:(rom_latency + multiply_latency)
    ~infer_output_precision:M.infer_output_precision
    ~create:M.create
    ~propagate_input_overflows:None
    ~shared
    [ dividend; pow ]
;;

let verify
  ?(shared = false)
  ?label
  ~base
  ~pow_base_bounds
  ~input_i_bits
  ~rom_latency
  ~multiply_latency
  ~dividend
  ~quotient
  ~pow
  ()
  =
  let pow_base_bounds = Inclusive_integer_range.(-pow_base_bounds) in
  (* Open some standard functions to use. *)
  let ( ~- ) = Fixed_op.negate in
  let mux2 = Fixed_op.mux2 in
  let ( <= ) = Fixed_compare.lte in
  let ( = ) = Fixed_compare.equals in
  let pow_precision = Inclusive_integer_range.precision pow_base_bounds in
  let zero_pow = Node.constant ~value:0 ~label:"zero_pow" ~precision:pow_precision () in
  let resize_to_pow = Resize.unsafe ~precision:pow_precision in
  let vdd = Node.constant ~value:1 ~label:"vdd" ~precision:Precision.bool () in
  let module M =
    Make (struct
      let base = base
      let pow_base_bounds = pow_base_bounds
      let output_i_bits = input_i_bits
      let rom_latency = rom_latency
      let multiply_latency = multiply_latency
      let check_for_error = false
    end)
  in
  (* Main node construction. *)
  mux2
    (pow <= zero_pow)
    (* If the original operation was multiplication, we don't have to verify anything
       here. *)
    vdd
    ((* When the original operation was division, we have to perform the inverse
        multiplication and check that it equals the original dividend. *)
     let pow = -pow |> resize_to_pow in
     let multiplication =
       Node.generic_function_single
         ?label
         ~tag
         ~latency:(rom_latency + multiply_latency)
         ~infer_output_precision:M.infer_output_precision
         ~create:M.create
         ~propagate_input_overflows:None
         ~shared
         [ quotient; pow ]
     in
     multiplication = dividend)
;;

let is_divisible
  ?(shared = false)
  ?label
  ~base
  ~pow_base_bounds
  ~input_i_bits
  ~rom_latency
  ~multiply_latency
  ~dividend
  ~pow
  ()
  =
  let module M =
    Make (struct
      let base = base
      let pow_base_bounds = pow_base_bounds
      let output_i_bits = input_i_bits (* Prevent overflow *)
      let rom_latency = rom_latency
      let multiply_latency = multiply_latency
      let check_for_error = true
    end)
  in
  let inputs = [ dividend; pow ] in
  M.validate_input_precisions (List.map inputs ~f:Node.precision);
  Node.generic_function_single
    ?label
    ~tag
    ~latency:(rom_latency + multiply_latency)
    ~infer_output_precision:(Fn.const Precision.bool)
    ~create:M.create_is_divisible_only
    ~propagate_input_overflows:None
    ~shared
    inputs
;;
