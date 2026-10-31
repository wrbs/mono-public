open Core
open Hardcaml
open Hardcaml_computation_dsl

module Make (X : Hardcaml_computation_dsl.Interface_with_precision.S) = struct
  let of_list l =
    List.map l ~f:X.to_alist
    |> List.transpose_exn
    |> List.map ~f:(fun l ->
      let tags, values = List.unzip l in
      List.hd_exn tags, values)
    |> X.of_alist
  ;;

  let distribute_valids (l : (_, _ X.t) With_valid.t2 list) =
    List.map l ~f:(fun { valid; value } ->
      X.map value ~f:(fun value -> { With_valid.valid; value }))
  ;;

  let collect_valids (t : _ With_valid.t X.t) =
    { With_valid.valid =
        (match X.to_list t with
         | { valid; _ } :: _ -> valid
         | [] -> raise_s [%message "[priority_select] interface has no fields"])
    ; value = X.map t ~f:(fun { valid = _; value } -> value)
    }
  ;;

  let onehot_select ?label (l : (_, _ X.t) With_valid.t2 list) =
    distribute_valids l
    |> of_list
    |> X.map2 X.port_names ~f:(fun port_name nodes ->
      let label = Option.map label ~f:(fun v -> [%string "%{v}_%{port_name}"]) in
      Select.onehot_select ?label nodes)
  ;;

  let priority_select ?label (l : (_, _ X.t) With_valid.t2 list) =
    distribute_valids l
    |> of_list
    |> X.map2 X.port_names ~f:(fun port_name nodes ->
      let label = Option.map label ~f:(fun v -> [%string "%{v}_%{port_name}"]) in
      Select.priority_select ?label nodes)
    |> collect_valids
  ;;

  let priority_select_with_default ?label (l : (_, _ X.t) With_valid.t2 list) ~default =
    distribute_valids l
    |> of_list
    |> X.map3 X.port_names default ~f:(fun port_name default nodes ->
      let label = Option.map label ~f:(fun v -> [%string "%{v}_%{port_name}"]) in
      Select.priority_select_with_default ?label nodes ~default)
  ;;

  let mux ?label case (l : _ X.t list) =
    of_list l
    |> X.map2 X.port_names ~f:(fun port_name nodes ->
      let label = Option.map label ~f:(fun v -> [%string "%{v}_%{port_name}"]) in
      Fixed_op.mux ?label case nodes)
  ;;

  let mux2 ?label cond if_true if_false = mux ?label cond [ if_false; if_true ]

  let zero =
    X.map2 X.port_names X.precision ~f:(fun name precision ->
      Node.constant ~value:0 ~label:[%string "%{name}_zero"] ~precision ())
  ;;

  let pipeline ~label ~n x =
    X.map2 X.port_names x ~f:(fun port_name node ->
      let label = [%string "%{label}_%{port_name}"] in
      Fixed_op.pipeline ~label ~n node)
  ;;
end
