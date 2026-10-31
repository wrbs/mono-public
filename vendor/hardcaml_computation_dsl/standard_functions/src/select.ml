open Core
open Hardcaml
open Hardcaml_computation_dsl
open Function

module Make (Arg : sig
    val tag : Function_tag.t
  end) =
struct
  open Arg

  let validate_input_precisions_and_return_output_precision
    ~which
    (input_precisions : Precision.t list)
    =
    let n = List.length input_precisions in
    if n % 2 <> 0
    then
      raise_s
        [%message
          "Select has to have even number of inputs! (With_valid list)"
            (tag : Function_tag.t)];
    let #(valids, values) = List.split_n input_precisions (n / 2) in
    (match List.all_equal valids ~equal:Precision.equal with
     | Some v ->
       if not (Precision.equal v Precision.bool)
       then raise_s [%message "Valids have to all be booleans!"]
     | None ->
       raise_s
         [%message
           "Valids in [Select] don't have the same precisions! Check your inputs and \
            resize them appropriately."
             (valids : Precision.t list)]);
    let value_precision =
      match List.all_equal values ~equal:Precision.equal with
      | Some v -> v
      | None ->
        raise_s
          [%message
            "Values in [Select] don't have the same precisions! Check your inputs and \
             resize them appropriately."
              (values : Precision.t list)]
    in
    match which with
    | `Valid -> Precision.bool
    | `Value -> value_precision
  ;;

  let create ~input_precisions (inputs : Input.t) =
    let _output_precision =
      validate_input_precisions_and_return_output_precision input_precisions
    in
    assert (List.length inputs = List.length input_precisions);
    let n = List.length inputs in
    let #(valids, values) = List.split_n inputs (n / 2) in
    let with_valids =
      List.map2_exn valids values ~f:(fun valid value -> { With_valid.valid; value })
    in
    match tag with
    | Onehot_select ->
      { With_valid.valid = Signal.vdd; value = Signal.onehot_select with_valids }
    | Priority_select -> Signal.priority_select with_valids
    | _ -> failwith "impossible"
  ;;

  let propagate_input_overflows (inputs : Function.Input.Full.t) =
    let open Signal in
    let n = List.length inputs in
    let #(valids, values) = List.split_n inputs (n / 2) in
    let with_valids =
      List.map2_exn
        valids
        values
        ~f:(fun { q = valid; overflow = _ } { overflow = value; q = _ } ->
          { With_valid.valid; value })
    in
    let value_overflowed =
      match tag with
      | Onehot_select -> onehot_select with_valids
      | Priority_select -> priority_select_with_default with_valids ~default:gnd
      | _ -> failwith "impossible"
    in
    let valids_overflow =
      List.map valids ~f:(fun v -> v.overflow) |> concat_lsb |> any_bit_set
    in
    { With_valid.valid = valids_overflow; value = value_overflowed }
  ;;

  let with_valid_node ?label (with_valids : _ With_valid.t list) =
    let valids, values =
      List.map with_valids ~f:(fun { valid; value } -> valid, value) |> List.unzip
    in
    let create_valid _scope ~clocking:_ ~build_mode:_ ~input_precisions (inputs : Input.t)
      =
      { Data.q = (create ~input_precisions inputs).valid; overflow = Signal.gnd }
    in
    let create_value _scope ~clocking:_ ~build_mode:_ ~input_precisions (inputs : Input.t)
      =
      { Data.q = (create ~input_precisions inputs).value; overflow = Signal.gnd }
    in
    let propagate_input_overflows_valid =
      Some (fun ~clocking:_ inputs -> (propagate_input_overflows inputs).valid)
    in
    let propagate_input_overflows_value =
      Some (fun ~clocking:_ inputs -> (propagate_input_overflows inputs).value)
    in
    let tag = Function_tag.to_string tag in
    { With_valid.valid =
        Node.generic_function_single
          ?label:(Option.map label ~f:(fun v -> v ^ "_valid"))
          ~tag
          ~latency:0
          ~infer_output_precision:
            (validate_input_precisions_and_return_output_precision ~which:`Valid)
          ~create:create_valid
          ~propagate_input_overflows:propagate_input_overflows_valid
          ~shared:false
          (valids @ values)
    ; value =
        Node.generic_function_single
          ?label:(Option.map label ~f:(fun v -> v ^ "_value"))
          ~tag
          ~latency:0
          ~infer_output_precision:
            (validate_input_precisions_and_return_output_precision ~which:`Value)
          ~create:create_value
          ~propagate_input_overflows:propagate_input_overflows_value
          ~shared:false
          (valids @ values)
    }
  ;;
end

let onehot_select ?label with_valids =
  let open Make (struct
      let tag = Function_tag.Onehot_select
    end) in
  (with_valid_node ?label with_valids).value
;;

let priority_select ?label (with_valids : _ With_valid.t list) =
  let open Make (struct
      let tag = Function_tag.Priority_select
    end) in
  with_valid_node ?label with_valids
;;

let priority_select_with_default ?label with_valids ~default =
  let res =
    priority_select
      ?label:(Option.map label ~f:(fun v -> v ^ "_priority_select"))
      with_valids
  in
  Fixed_op.mux2
    ?label:(Option.map label ~f:(fun v -> v ^ "_mux2"))
    res.valid
    res.value
    default
;;
