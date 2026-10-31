open! Core

module G = struct
  type 'a t = 'a Defs.Grammar.t [@@deriving sexp_of]

  let return x : _ t = Return x
  let union ts : _ t = Union ts
  let concat a b ~f : _ t = Append (a, b, f)
  let star t ~init ~f : _ t = Star (t, { init; f })
  let star_unit t : _ t = Star_unit t
  let concat_unit (ts : unit t list) : unit t = Concat_unit ts
end

module How_to_fold = struct
  type ('acc, 'item) t =
    | Unit : (unit, unit) t
    | Fold :
        { init : 'acc
        ; f : 'acc -> 'item -> 'acc
        }
        -> ('acc, 'item) t

  let return (type acc item) : (acc, item) t -> acc G.t = function
    | Unit -> G.return ()
    | Fold { init; _ } -> G.return init
  ;;

  let append (type acc item) : (acc, item) t -> acc G.t -> item G.t -> acc G.t = function
    | Unit -> fun acc item -> G.concat_unit [ acc; item ]
    | Fold { f; _ } -> fun acc item -> G.concat acc item ~f
  ;;

  let star (type acc item) : (acc, item) t -> item G.t -> acc G.t = function
    | Unit -> fun item -> G.star_unit item
    | Fold { f; init } -> fun item -> G.star item ~init ~f
  ;;
end

module Spec = struct
  type ('acc, 'item) t =
    { how_to_fold : ('acc, 'item) How_to_fold.t
    ; bounds : Bound.t * Bound.t
    ; item : 'item Defs.Grammar.t
    ; sep : unit Defs.Grammar.t option
    }
end

(** How to interpret [items_tail_end]: take the list and infinitely extend it with its
    last item. The grammar matches the reverse of any prefix of this infinite list that
    respects the given bounds.

    For example, say the input list is [ [c; b; a] ]. The extended list would be
    [ c :: b :: a :: a :: a's_forever ]. And an unbounded fold would match [[]], [[ c ]],
    [[b; c]], [[a; b; c]], [[a; a; b; c]] etc... *)
let build_grammar_no_sep
  (type acc item)
  ~(how_to_fold : (acc, item) How_to_fold.t)
  ~from_incl
  ~to_excl
  ~(items_tail_end : item G.t Nonempty_list.t)
  : acc G.t
  =
  let from_incl = Int.max 0 from_incl in
  let bounds_conflict =
    match to_excl with
    | Some to_excl -> to_excl <= from_incl
    | None -> false
  in
  if bounds_conflict
  then G.union []
  else (
    let rec build ~from_incl ~to_excl ~items_tail_end =
      match from_incl, to_excl, (items_tail_end : _ Nonempty_list.t) with
      | 0, Some 1, _ -> How_to_fold.return how_to_fold
      | 0, None, [ item ] -> How_to_fold.star how_to_fold item
      | _, _, last_item :: items_before_that ->
        let repeated =
          How_to_fold.append
            how_to_fold
            (build
               ~from_incl:(Int.max 0 (from_incl - 1))
               ~to_excl:(Option.map ~f:Int.pred to_excl)
               ~items_tail_end:
                 (Nonempty_list.of_list items_before_that
                  |> Option.value ~default:items_tail_end))
            last_item
        in
        if from_incl = 0
        then G.union [ repeated; How_to_fold.return how_to_fold ]
        else repeated
    in
    build ~from_incl ~to_excl ~items_tail_end)
;;

let build_grammar
  (type acc item)
  ({ how_to_fold; bounds = from, to_; sep; item } : (acc, item) Spec.t)
  : acc G.t
  =
  let from_incl =
    match from with
    | Incl n -> n
    | Excl n -> n + 1
    | Unbounded -> 0
  in
  let to_excl =
    match to_ with
    | Incl n -> Some (n + 1)
    | Excl n -> Some n
    | Unbounded -> None
  in
  let items_tail_end : _ Nonempty_list.t =
    match sep with
    | None -> [ item ]
    | Some sep ->
      let item_sep : item G.t =
        match how_to_fold with
        | Unit -> G.concat_unit [ item; sep ]
        | Fold _ -> G.concat item sep ~f:(fun x () -> x)
      in
      [ item; item_sep ]
  in
  build_grammar_no_sep ~how_to_fold ~from_incl ~to_excl ~items_tail_end
;;
