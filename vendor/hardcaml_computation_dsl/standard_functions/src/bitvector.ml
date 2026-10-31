open Core
open Hardcaml
module F = Hardcaml_computation_dsl.Function
module N = Hardcaml_computation_dsl.Node
module Precision = Hardcaml_computation_dsl.Precision

let validate_precision ~allow_signed (p : Precision.t) =
  if ((not allow_signed) && p.signed) || p.f_bits <> 0 || p.i_bits <= 0
  then
    raise_s
      [%message
        "Invalid precision for bitvector nodes! Must be a non-zero-width unsigned  \
         integer."
          (allow_signed : bool)
          (p : Precision.t)]
;;

let concat_lsb ?label (input_nodes : N.t list) =
  let create
    (_ : Scope.t)
    ~clocking:_
    ~build_mode:_
    ~input_precisions:_
    (inputs : F.Input.t)
    =
    let open Signal in
    { F.Data.q = concat_lsb inputs; overflow = gnd }
  in
  N.generic_function_single
    ?label
    ~tag:"concat"
    ~latency:0
    ~infer_output_precision:(fun (precisions : Precision.t list) ->
      let total_bits =
        List.sum (module Int) precisions ~f:(fun p ->
          validate_precision ~allow_signed:false p;
          p.i_bits)
      in
      { Precision.signed = false; i_bits = total_bits; f_bits = 0 })
    ~create
    ~propagate_input_overflows:None
    ~shared:false
    input_nodes
;;

let select ?label ?(allow_signed = false) ~high ~low (input_node : N.t) =
  let create
    (_ : Scope.t)
    ~clocking:_
    ~build_mode:_
    ~input_precisions:_
    (inputs : F.Input.t)
    =
    let open Signal in
    let input =
      match inputs with
      | [ hd ] -> hd
      | _ -> failwith "Expected exactly one input"
    in
    { F.Data.q = select ~high ~low input; overflow = gnd }
  in
  N.generic_function_single
    ?label
    ~tag:"select"
    ~latency:0
    ~infer_output_precision:(fun (precisions : Precision.t list) ->
      List.iter precisions ~f:(validate_precision ~allow_signed);
      { Precision.i_bits = high - low + 1; f_bits = 0; signed = false })
    ~create
    ~propagate_input_overflows:None
    ~shared:false
    [ input_node ]
;;

let drop_top ?label ?allow_signed ~width n =
  let original_width = Precision.width (N.precision n) in
  select ?allow_signed ?label ~high:(original_width - width - 1) ~low:0 n
;;

let sel_top ?label ?allow_signed ~width n =
  let original_width = Precision.width (N.precision n) in
  select ?allow_signed ?label ~high:(original_width - 1) ~low:(original_width - width) n
;;

let drop_bottom ?label ?allow_signed ~width n =
  let original_width = Precision.width (N.precision n) in
  select ?allow_signed ?label ~high:(original_width - 1) ~low:width n
;;

let sel_bottom ?label ?allow_signed ~width n =
  select ?allow_signed ?label ~high:(width - 1) ~low:0 n
;;

let bitwise_not ?label ?(allow_signed = false) (input_node : N.t) =
  let create
    (_ : Scope.t)
    ~clocking:_
    ~build_mode:_
    ~input_precisions:_
    (inputs : F.Input.t)
    =
    let open Signal in
    let input =
      match inputs with
      | [ hd ] -> hd
      | _ -> failwith "Expected one input"
    in
    { F.Data.q = ~:input; overflow = gnd }
  in
  N.generic_function_single
    ?label
    ~tag:"bitwise_not"
    ~latency:0
    ~infer_output_precision:(fun (precisions : Precision.t list) ->
      match precisions with
      | [ p ] ->
        validate_precision ~allow_signed p;
        p
      | _ -> failwith "Expected one input precision")
    ~create
    ~propagate_input_overflows:None
    ~shared:false
    [ input_node ]
;;

let bitwise_binop
  ~tag
  ~op
  ?label
  ?(allow_signed = false)
  (input_node1 : N.t)
  (input_node2 : N.t)
  =
  let create
    (_ : Scope.t)
    ~clocking:_
    ~build_mode:_
    ~input_precisions:_
    (inputs : F.Input.t)
    =
    let open Signal in
    let input1, input2 =
      match inputs with
      | [ a; b ] -> a, b
      | _ -> failwith "Expected exactly two inputs"
    in
    { F.Data.q = op input1 input2; overflow = gnd }
  in
  N.generic_function_single
    ?label
    ~tag
    ~latency:0
    ~infer_output_precision:(fun (precisions : Precision.t list) ->
      match precisions with
      | [ p1; p2 ] ->
        validate_precision ~allow_signed p1;
        validate_precision ~allow_signed p2;
        if not (Precision.equal p1 p2)
        then
          raise_s
            [%message
              "Bitwise operations require inputs to have the same precision"
                (p1 : Precision.t)
                (p2 : Precision.t)];
        p1
      | _ -> failwith "Expected two input precisions")
    ~create
    ~propagate_input_overflows:None
    ~shared:false
    [ input_node1; input_node2 ]
;;

let bitwise_or ?label ?allow_signed input_node1 input_node2 =
  bitwise_binop
    ~tag:"bitwise_or"
    ~op:Signal.( |: )
    ?label
    ?allow_signed
    input_node1
    input_node2
;;

let bitwise_and ?label ?allow_signed input_node1 input_node2 =
  bitwise_binop
    ~tag:"bitwise_and"
    ~op:Signal.( &: )
    ?label
    ?allow_signed
    input_node1
    input_node2
;;
