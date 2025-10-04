@@ portable

open Core

(** This module is the implementation of table sorting that powers the Basic PRT, and the
    Workflow UI table.

    Importantly, it is only one of many possible ways to implement sorting: PRT's expert
    API does not enforce any particular sort implementation. For example, this module
    doesn't allow custom directions beyond [`Asc] and [`Desc]. *)

module Sort_kind : sig
  type ('key, 'data) sort := 'key * 'data -> 'key * 'data -> int

  type ('key, 'data) t =
    { forward : ('key, 'data) sort
    ; reverse : ('key, 'data) sort
    }

  val reversible : forward:('key, 'data) sort -> ('key, 'data) t
  val reversible' : reverse:('key, 'data) sort -> ('key, 'data) t
end

module Dir : sig
  type t =
    [ `Asc
    | `Desc
    ]
  [@@deriving sexp, bin_io, equal, compare]
end

module Sort_state : sig
  type t =
    | Not_sortable
    | Not_sorted
    | Single_sort of Dir.t
    | Multi_sort of
        { dir : Dir.t
        ; index : int
        }
end

module Order : sig
  type 'col_id t = ('col_id * Dir.t) list [@@deriving sexp, bin_io, equal, compare]

  (** Describes the behavior when cycling the sort direction of a column. *)
  module Cycle_behavior : sig
    type t =
      | Asc_to_desc_to_none
      | Desc_to_asc_to_none
  end

  module Action : sig
    (** Simple variant module used as a message for changing ordering. *)
    type 'col_id t =
      | Set_sort of 'col_id * Cycle_behavior.t
      | Set_sort_with_dir of 'col_id * Dir.t option
      | Add_sort of 'col_id * Cycle_behavior.t
    [@@deriving sexp_of]
  end

  (** Performs ordering change. {!Action.Set_sort} sets order on one column;
      {!Action.Add_sort} maintains multiple column ordering, when the newest added sorting
      is the most priority. {!Action.Set_sort_with_dir} sets order on one column to a
      specified direction, or no sorting.

      The order on one column for [Set_sort] and [Add_sort] is changed in a cycle fashion:
      [Ascending -> Descending -> None -> Ascending]. *)
  val apply_action
    :  'col_id t
    -> equal:('col_id -> 'col_id -> bool)
    -> 'col_id Action.t
    -> 'col_id t

  type ('key, 'data) compare := 'key * 'data -> 'key * 'data -> int

  val to_compare
    :  ?override_sort:(('key, 'data) compare -> ('key, 'data) compare)
    -> 'col_id t
    -> sorters:('col_id, ('key, 'data) Sort_kind.t, 'col_cmp) Map.t
    -> default_sort:('key, 'data) compare option
    -> ('key, 'data, 'row_cmp) Incr_map_collate.Compare.t

  val default : 'col_id t
end

module Stable : sig
  module Order : sig
    module V1 : sig
      type nonrec 'col_id t = 'col_id Order.t [@@deriving sexp, bin_io, equal, compare]
    end
  end
end
