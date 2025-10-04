open! Core
open! Bonsai_web
open Incr_map_collate
open Bonsai_web_ui_partial_render_table
open Bonsai_bench
open Bonsai.Let_syntax
module Table = Expert

module Action = struct
  type ('key, 'column_id) t =
    | Unfocus
    | Focus_up
    | Focus_down
    | Focus_left
    | Focus_right
    | Page_up
    | Page_down
    | Focus of ('key * 'column_id)
  [@@deriving sexp, equal]
end

module Input = struct
  type ('key, 'column_id, 'data, 'cmp) t =
    { filter : (key:'key -> data:'data -> bool) option Bonsai.Var.t
    ; order : ('key, 'data, 'cmp) Incr_map_collate.Compare.t Bonsai.Var.t
    ; rank_range : int Collate.Which_range.t Bonsai.Var.t
    ; key_range : 'key Collate.Which_range.t Bonsai.Var.t
    ; map : ('key, 'data, 'cmp) Map.t Bonsai.Var.t
    ; on_change : (('key * 'column_id) option -> unit Effect.t) Bonsai.Var.t
    }

  let create
    ?(filter = None)
    ?(order = Compare.Unchanged)
    ?(rank_range = Collate.Which_range.To 100)
    ?(key_range = Collate.Which_range.All_rows)
    ?(on_change = Fn.const Effect.Ignore)
    map
    =
    { filter = Bonsai.Var.create filter
    ; order = Bonsai.Var.create order
    ; rank_range = Bonsai.Var.create rank_range
    ; key_range = Bonsai.Var.create key_range
    ; map = Bonsai.Var.create map
    ; on_change = Bonsai.Var.create on_change
    }
  ;;

  let apply_filter t filter = Interaction.change_input t.filter (Some filter)
  let clear_filter t = Interaction.change_input t.filter None
  let set_map t map = Interaction.change_input t.map map
  let set_order t order = Interaction.change_input t.order order
  let set_rank_range t rank_range = Interaction.change_input t.rank_range rank_range
  let set_on_change t on_change = Interaction.change_input t.on_change on_change

  let scroll t ~start ~stop ~window_size =
    let stride = if start > stop then -1 else 1 in
    List.range ~stride start stop
    |> List.map ~f:(fun i ->
         Interaction.change_input
           t.rank_range
           (Collate.Which_range.Between (i, i + window_size - 1)))
    |> Interaction.many_with_stabilizations
  ;;
end

let component_for_bench
  ?preload_rows
  comparator
  ~columns
  { Input.filter; order; rank_range; key_range; map; on_change }
  =
  let filter = Bonsai.Var.value filter in
  let order = Bonsai.Var.value order in
  let rank_range = Bonsai.Var.value rank_range in
  let key_range = Bonsai.Var.value key_range in
  let map = Bonsai.Var.value map in
  let on_change = Bonsai.Var.value on_change in
  let%sub collate, key_rank =
    let collate =
      let%map filter = filter
      and order = order
      and rank_range = rank_range
      and key_range = key_range in
      { Collate.filter; order; key_range; rank_range }
    in
    Table.collate
      ~filter_equal:phys_equal
      ~filter_to_predicate:Fn.id
      ~order_equal:phys_equal
      ~order_to_compare:Fn.id
      map
      collate
  in
  Table.component
    ?preload_rows
    comparator
    ~focus:(Table.Focus.By_cell { on_change; compute_presence = return; key_rank })
    ~row_height:(Value.return (`Px 1))
    ~columns
    collate
;;

let create_bench ?preload_rows comparator ~initial_vars ~columns ~interaction ~test_name =
  let interaction = interaction initial_vars in
  let component = component_for_bench comparator ?preload_rows ~columns initial_vars in
  let module Focus_control = Table.Focus.By_cell in
  let get_inject { Table.Result.focus; _ } = function
    | Action.Unfocus -> Focus_control.unfocus focus
    | Focus_up -> Focus_control.focus_up focus
    | Focus_down -> Focus_control.focus_down focus
    | Focus_left -> Focus_control.focus_left focus
    | Focus_right -> Focus_control.focus_right focus
    | Page_up -> Focus_control.page_up focus
    | Page_down -> Focus_control.page_down focus
    | Focus (key, column_id) -> (Focus_control.focus focus) key column_id
  in
  Bonsai_bench.create ~name:test_name ~component ~get_inject interaction
;;
