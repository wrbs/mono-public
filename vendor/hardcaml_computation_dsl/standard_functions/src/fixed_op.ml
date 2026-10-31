open Core
open Hardcaml
module Clocking = Clocking
open Hardcaml_computation_dsl

module Op = struct
  include Function_tag.Fixed_op.Op

  let fixed_binop_is_signed = function
    | [ (p0 : Precision.t); p1 ] -> p0.signed || p1.signed
    | _ as input_precisions ->
      raise_s
        [%message
          "[Fixed_op] binary operation expects two inputs."
            (input_precisions : Precision.t list)]
  ;;

  let assert_all_equal_and_return t (input_precisions : Precision.t list) =
    match List.all_equal input_precisions ~equal:Precision.equal with
    | Some v -> v
    | None ->
      raise_s
        [%message
          "Inputs to [Fixed_op] don't have the same precisions! Check your inputs and \
           resize them appropriately."
            (t : t)
            (input_precisions : Precision.t list)]
  ;;

  let assert_is_logic t v =
    if not (Precision.equal v Precision.bool)
    then failwith [%string "%{to_string t} condition must be single-bit value"]
  ;;

  let validate_input_precisions_and_return_output_precision t input_precisions =
    match t with
    | Add | Subtract ->
      let input_signed = fixed_binop_is_signed input_precisions in
      let signed =
        (* Subtraction always results in a signed value - because 2's complement is nice,
           this doesn't require any special handling; we can just subtract unsigned
           inputs, allow the overflow, and treat the result as signed. *)
        match t with
        | Add -> input_signed
        | Subtract -> true
        | _ -> failwith "impossible"
      in
      let max_i_bits =
        List.fold ~init:0 input_precisions ~f:(fun acc { signed; i_bits; _ } ->
          let maybe_signed_i_bits =
            if input_signed && not signed then i_bits + 1 else i_bits
          in
          Int.max acc maybe_signed_i_bits)
      in
      let max_f_bits =
        List.fold ~init:0 input_precisions ~f:(fun acc { f_bits; _ } ->
          Int.max acc f_bits)
      in
      let output_precision =
        match input_precisions with
        | [ _; _ ] -> { Precision.signed; i_bits = max_i_bits + 1; f_bits = max_f_bits }
        | _ -> failwith [%string "%{to_string t} has to have exactly 2 inputs"]
      in
      ~output_precision, ~input_signed
    | Multiply ->
      let input_signed = fixed_binop_is_signed input_precisions in
      let sum_i_bits =
        List.fold ~init:0 input_precisions ~f:(fun acc { signed; i_bits; _ } ->
          let maybe_signed_i_bits =
            if input_signed && not signed then i_bits + 1 else i_bits
          in
          acc + maybe_signed_i_bits)
      in
      let sum_f_bits =
        List.fold ~init:0 input_precisions ~f:(fun acc { f_bits; _ } -> acc + f_bits)
      in
      let output_precision =
        match input_precisions with
        | [ _; _ ] ->
          { Precision.signed = input_signed; i_bits = sum_i_bits; f_bits = sum_f_bits }
        | _ -> failwith [%string "%{to_string t} has to have exactly 2 inputs"]
      in
      ~output_precision, ~input_signed
    | Div_by_2 ->
      let output_precision =
        match input_precisions with
        | [ p ] ->
          if p.i_bits = 0
          then
            failwith
              [%string
                "%{to_string t} input must have at least one integer bit! %{p#Precision}"];
          { p with i_bits = p.i_bits - 1; f_bits = p.f_bits + 1 }
        | _ -> failwith [%string "%{to_string t} only works on single input!"]
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Pipeline ->
      let output_precision =
        match input_precisions with
        | [ p ] -> p
        | _ -> failwith [%string "%{to_string t} only works on single input!"]
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Negate ->
      let output_precision =
        match input_precisions with
        | [ p ] ->
          if not p.signed then raise_s [%message "Cannot negate unsigned value!"];
          { p with i_bits = p.i_bits + 1 }
        | _ -> failwith [%string "%{to_string t} only works on single input!"]
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Mux ->
      let output_precision =
        match input_precisions with
        | [] | [ _ ] -> failwith [%string "%{to_string t} has to have >= 2 inputs"]
        | case :: values ->
          if case.signed || case.f_bits <> 0
          then
            raise_s
              [%message "mux has to have unsigned integer case!" (case : Precision.t)];
          assert_all_equal_and_return t values
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Logical_and | Logical_or ->
      let output_precision =
        match input_precisions with
        | [ a; b ] ->
          assert_is_logic t a;
          assert_is_logic t b;
          Precision.bool
        | _ -> failwith [%string "%{to_string t} has to have exactly 2 inputs"]
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Logical_not ->
      let output_precision =
        match input_precisions with
        | [ a ] ->
          assert_is_logic t a;
          Precision.bool
        | _ -> failwith [%string "%{to_string t} has to have exactly 1 inputs"]
      in
      ~output_precision, ~input_signed:output_precision.signed
    | Inverse_lookup _ ->
      failwith
        "impossible. in order to construct a node with [op = Inverse_lookup], use the \
         function provided in the dedicated [inverse_lookup_rom_node.ml] file. The \
         operation is included in the [Op.t] variant because it is a fundamental \
         mathematical operation on fixed types, but it is implemented very differently \
         from the operations in this file."
  ;;
end

let rec pipeline_with_name scope ~name ?(attributes = []) clocking ~n ?enable d =
  let ( -- ) = Scope.naming scope in
  let maybe_add_attributes s = List.fold attributes ~init:s ~f:Signal.add_attribute in
  if n < 0
  then raise_s [%message "[pipeline_with_name] cannot accept a negative value" (n : int)]
  else if n = 0
  then d
  else
    maybe_add_attributes
      (Clocking.reg
         clocking
         ?enable
         (pipeline_with_name scope ~name ~attributes clocking ~n:(n - 1) ?enable d))
    -- [%string "%{name}_pipeline_%{n#Int}"]
;;

module Make (X : sig
    val label : string option
    val op : Op.t
    val latency : int
    val attributes : Rtl_attribute.t list
  end) =
struct
  open X

  let () =
    match op with
    | Pipeline -> ()
    | _ -> assert (latency = 0)
  ;;

  let tag =
    let open Function_tag in
    Fixed_op { op; latency } |> to_string
  ;;

  let infer_output_precision l =
    let ~output_precision, ~input_signed:_ =
      Op.validate_input_precisions_and_return_output_precision op l
    in
    output_precision
  ;;

  let validate_input_precisions input_precisions =
    ignore
      (Op.validate_input_precisions_and_return_output_precision op input_precisions
       : output_precision:Precision.t * input_signed:bool)
  ;;

  let create
    scope
    ~(clocking : _ Clocking.t)
    ~build_mode:_
    ~input_precisions
    (inputs : Function.Input.t)
    =
    (* Initial inference/input validation. *)
    assert (List.length inputs = List.length input_precisions);
    let ~output_precision:_, ~input_signed =
      Op.validate_input_precisions_and_return_output_precision op input_precisions
    in
    (* Create the appropriate underlying [Hardcaml_fixed_point] module. *)
    let module Fixed_point =
      (val if input_signed
           then
             (module Hardcaml_fixed_point.Signed (Signal)
             : Hardcaml_fixed_point.Fixed_point
               with type bits = Signal.t)
           else (module Hardcaml_fixed_point.Unsigned (Signal)))
    in
    let fp_inputs =
      List.map2_exn inputs input_precisions ~f:(fun s { signed; f_bits; i_bits = _ } ->
        let s = if input_signed && not signed then Signal.(gnd @: s) else s in
        Fixed_point.create f_bits s)
    in
    let logical ~op =
      match inputs with
      | [ a; b ] -> op a b |> Fixed_point.create 0
      | _ -> assert false
    in
    let q =
      match op with
      | Add | Subtract | Multiply ->
        let op =
          match op with
          | Add -> Fixed_point.( +: )
          | Subtract -> Fixed_point.( -: )
          | Multiply -> Fixed_point.( *: )
          | _ -> assert false
        in
        (match fp_inputs with
         | [ a; b ] -> op a b
         | _ -> assert false)
      | Div_by_2 -> Fixed_point.scale_pow2 (List.hd_exn fp_inputs) (-1)
      | Mux -> Fixed_point.mux (List.hd_exn inputs) (List.tl_exn fp_inputs)
      | Pipeline ->
        List.hd_exn fp_inputs
        |> Fixed_point.map
             ~f:
               (pipeline_with_name
                  scope
                  ~attributes
                  ~name:(Option.value_exn label)
                  clocking
                  ~n:latency)
      | Negate ->
        (match fp_inputs with
         | [ fp_input ] ->
           let fp_zero =
             Fixed_point.of_float
               (Fixed_point.width_int fp_input)
               (Fixed_point.width_frac fp_input)
               0.
           in
           Fixed_point.( -: ) fp_zero fp_input
         | _ -> assert false)
      | Logical_and -> logical ~op:Signal.( &: )
      | Logical_or -> logical ~op:Signal.( |: )
      | Logical_not -> Signal.( ~: ) (List.hd_exn inputs) |> Fixed_point.create 0
      | Inverse_lookup _ -> failwith "separate file"
    in
    (* All of these operations resize appropriately so as not to overflow - the user will
       use [resize] to potentially overflow their operations, afterwards. *)
    { Function.Data.q = Fixed_point.signal q; overflow = Signal.gnd }
  ;;

  let propagate_input_overflows =
    match op with
    | Mux ->
      Some
        (fun ~clocking:_ (inputs : Function.Input.Full.t) ->
          let data_overflow =
            Signal.mux
              (List.hd_exn inputs).q
              (List.tl_exn inputs |> List.map ~f:(fun v -> v.overflow))
          in
          Signal.( |: ) (List.hd_exn inputs).overflow data_overflow)
    | _ -> None
  ;;
end

let node ~op ?(shared = false) ?label ?(attributes = []) ~latency inputs =
  let open Make (struct
      let label = label
      let op = op
      let latency = latency
      let attributes = attributes
    end) in
  validate_input_precisions (List.map inputs ~f:Node.precision);
  Node.generic_function_single
    ?label
    ~tag
    ~latency
    ~infer_output_precision
    ~create
    ~propagate_input_overflows
    ~shared
    inputs
;;

let add ?shared ?label a b = node ~op:Add ?shared ?label ~latency:0 [ a; b ]
let subtract ?shared ?label a b = node ~op:Subtract ?shared ?label ~latency:0 [ a; b ]
let multiply ?shared ?label a b = node ~op:Multiply ?shared ?label ~latency:0 [ a; b ]
let div_by_2 ?label input = node ~op:Div_by_2 ~shared:false ?label ~latency:0 [ input ]
let mux ?label cond values = node ~op:Mux ~shared:false ?label ~latency:0 (cond :: values)
let mux2 ?label cond if_true if_false = mux ?label cond [ if_false; if_true ]

let pipeline ?attributes ~label ~n input =
  if n < 0
  then raise_s [%message "[Fixed_op.pipeline] cannot accept a negative value" (n : int)];
  node ?attributes ~shared:false ~op:Pipeline ~label ~latency:n [ input ]
;;

let negate ?label input = node ~op:Negate ~shared:false ?label ~latency:0 [ input ]
let logical_and ?label a b = node ?label ~shared:false ~latency:0 ~op:Logical_and [ a; b ]
let logical_or ?label a b = node ?label ~shared:false ~latency:0 ~op:Logical_or [ a; b ]
let logical_not ?label a = node ?label ~shared:false ~latency:0 ~op:Logical_not [ a ]
