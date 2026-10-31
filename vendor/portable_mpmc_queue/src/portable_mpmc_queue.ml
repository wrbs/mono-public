(* Copyright © 2023 Vesa Karvonen
 *
 * Permission to use, copy, modify, and/or distribute this software for any purpose
 * with or without fee is hereby granted, provided that the above copyright notice
 * and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 *)

(* This implementation originated as picos_aux_mpmcq in Picos *)

open! Base
open! Portable_kernel
module Adaptive_backoff = Portable_adaptive_backoff

(** [log_scale] was picked empirically by benchmarking this queue with different
    [log_scale] values and picking a small value that seemed to give good performance. The
    optimal value should mostly depend on the cost of the critical sections between atomic
    loads and subsequent compare-and-sets and the cost of [cpu_relax] and those may
    potentially depend on the specific CPU model, but should be mostly independent from
    the code using the queue, because those critical sections do not run user code and
    don't access the user data. *)
let log_scale = 8

type ('a : value_or_null, _) queue : immutable_data with 'a @@ contended portable =
  | Cons :
      ('a : value_or_null).
      { counter : int
      ; value : 'a @@ contended portable
      ; suffix : 'a head
      }
      -> ('a, [> `Cons ]) queue
  | Head : ('a : value_or_null). { counter : int } -> ('a, [> `Head ]) queue
  | Snoc :
      ('a : value_or_null).
      { counter : int
      ; prefix : 'a tail
      ; value : 'a @@ contended portable
      }
      -> ('a, [> `Snoc ]) queue
  | Tail :
      ('a : value_or_null).
      { counter : int
      ; mutable move : ('a, [ `Snoc | `Used ]) queue
      }
      -> ('a, [> `Tail ]) queue
  | Used : ('a : value_or_null). ('a, [> `Used ]) queue
[@@unsafe_allow_any_mode_crossing]

and ('a : value_or_null) head =
  | H : ('a : value_or_null). ('a, [< `Cons | `Head ]) queue -> 'a head
[@@unboxed]

and ('a : value_or_null) tail =
  | T : ('a : value_or_null). ('a, [< `Snoc | `Tail ]) queue -> 'a tail
[@@unboxed]

(* We manually pad the record such that the whole record takes [16] words and the [head]
   and [tail] should be in two different [8] word cache lines to allow producers and
   consumer to mostly access the queue in parallel without contention as long as the queue
   is not empty. *)
type ('a : value_or_null) inner =
  { mutable head : 'a head [@atomic]
  ; random_key : int
  ; _pad3 : int
  ; _pad4 : int
  ; _pad5 : int
  ; _pad6 : int
  ; _pad7 : int
  ; _pad8 : int
  ; mutable tail : 'a tail [@atomic]
  ; _pad10 : int
  ; _pad11 : int
  ; _pad12 : int
  ; _pad13 : int
  ; _pad14 : int
  ; _pad15 : int
  }

type ('a : value_or_null) t = { inner : 'a inner @@ contended }

let create ?padded:_ () =
  let head = H (Head { counter = 1 }) in
  let tail = T (Tail { counter = 0; move = Used }) in
  let random_key = Int.of_int64_trunc (Random.bits64 ()) in
  { inner =
      { head
      ; random_key
      ; _pad3 = 0
      ; _pad4 = 0
      ; _pad5 = 0
      ; _pad6 = 0
      ; _pad7 = 0
      ; _pad8 = 0
      ; tail
      ; _pad10 = 0
      ; _pad11 = 0
      ; _pad12 = 0
      ; _pad13 = 0
      ; _pad14 = 0
      ; _pad15 = 0
      }
  }
;;

let rev = function
  | (Snoc { counter; prefix; value } : (_, [< `Snoc ]) queue) ->
    let[@inline] rec go (suffix : (_, [< `Cons ]) queue) = function
      | T (Snoc { counter; prefix; value }) ->
        go (Cons { counter; value; suffix = H suffix }) prefix
      | T (Tail _) -> suffix
    in
    go (Cons { counter; value; suffix = H (Head { counter = counter + 1 }) }) prefix
;;

let push t value =
  let[@inline] rec go t value = function
    | T (Snoc snoc_r) as prefix ->
      let after = Snoc { counter = snoc_r.counter + 1; prefix; value } in
      (match
         Atomic.Loc.compare_and_set
           [%atomic.loc t.inner.tail]
           ~if_phys_equal_to:prefix
           ~replace_with:(T after)
       with
       | Set_here -> ()
       | Compare_failed ->
         Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
         go t value (Atomic.Loc.get [%atomic.loc t.inner.tail]))
    | T (Tail tail_r) as prefix ->
      (match tail_r.move with
       | Used ->
         let after = Snoc { counter = tail_r.counter + 1; prefix; value } in
         (match
            Atomic.Loc.compare_and_set
              [%atomic.loc t.inner.tail]
              ~if_phys_equal_to:prefix
              ~replace_with:(T after)
          with
          | Set_here -> ()
          | Compare_failed ->
            Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
            go t value (Atomic.Loc.get [%atomic.loc t.inner.tail]))
       | Snoc move_r as move ->
         (match Atomic.Loc.get [%atomic.loc t.inner.head] with
          | H (Head head_r as head) when head_r.counter < move_r.counter ->
            let after = rev move in
            if phys_equal (Atomic.Loc.get [%atomic.loc t.inner.head]) (H head)
            then (
              match
                Atomic.Loc.compare_and_set
                  [%atomic.loc t.inner.head]
                  ~if_phys_equal_to:(H head)
                  ~replace_with:(H after)
              with
              | Set_here -> tail_r.move <- Used
              | Compare_failed -> ())
          | _ -> tail_r.move <- Used);
         Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
         go t value (Atomic.Loc.get [%atomic.loc t.inner.tail]))
  in
  (* Magic: elements are returned at once and popped or stolen exactly once. *)
  let value = (Obj.magic_many [@mode portable contended]) value in
  go t value (Atomic.Loc.get [%atomic.loc t.inner.tail])
;;

let push_head t value =
  let[@inline] rec go t value =
    match Atomic.Loc.get [%atomic.loc t.inner.head] with
    | H (Cons cons_r) as suffix ->
      let after = Cons { counter = cons_r.counter - 1; value; suffix } in
      (match
         Atomic.Loc.compare_and_set
           [%atomic.loc t.inner.head]
           ~if_phys_equal_to:suffix
           ~replace_with:(H after)
       with
       | Set_here -> ()
       | Compare_failed ->
         Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
         go t value)
    | H (Head head_r) as head ->
      (match Atomic.Loc.get [%atomic.loc t.inner.tail] with
       | T (Snoc snoc_r as move) ->
         if not (phys_equal (Atomic.Loc.get [%atomic.loc t.inner.head]) head)
         then go t value
         else if head_r.counter = snoc_r.counter
         then (
           let prefix = T (Snoc { snoc_r with value }) in
           let after = Snoc { snoc_r with counter = snoc_r.counter + 1; prefix } in
           match
             Atomic.Loc.compare_and_set
               [%atomic.loc t.inner.tail]
               ~if_phys_equal_to:(T move)
               ~replace_with:(T after)
           with
           | Set_here -> ()
           | Compare_failed ->
             Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
             go t value)
         else (
           let tail = Tail { counter = snoc_r.counter; move } in
           (match
              Atomic.Loc.compare_and_set
                [%atomic.loc t.inner.tail]
                ~if_phys_equal_to:(T move)
                ~replace_with:(T tail)
            with
            | Set_here -> ()
            | Compare_failed ->
              Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale);
           go t value)
       | T (Tail tail_r) as prefix ->
         (match tail_r.move with
          | Used ->
            if phys_equal (Atomic.Loc.get [%atomic.loc t.inner.head]) head
            then (
              let tail = Snoc { counter = tail_r.counter + 1; value; prefix } in
              match
                Atomic.Loc.compare_and_set
                  [%atomic.loc t.inner.tail]
                  ~if_phys_equal_to:prefix
                  ~replace_with:(T tail)
              with
              | Set_here -> ()
              | Compare_failed ->
                Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
                go t value)
            else go t value
          | Snoc move_r as move ->
            (match Atomic.Loc.get [%atomic.loc t.inner.head] with
             | H (Head head_r as head) when head_r.counter < move_r.counter ->
               let after = rev move in
               if phys_equal (Atomic.Loc.get [%atomic.loc t.inner.head]) (H head)
               then (
                 match
                   Atomic.Loc.compare_and_set
                     [%atomic.loc t.inner.head]
                     ~if_phys_equal_to:(H head)
                     ~replace_with:(H after)
                 with
                 | Compare_failed -> ()
                 | Set_here -> tail_r.move <- Used)
             | _ -> tail_r.move <- Used);
            go t value))
  in
  (* Magic: elements are returned at once and popped or stolen exactly once. *)
  let value = (Obj.magic_many [@mode portable contended]) value in
  go t value
;;

type ('a : value_or_null, _ : value_or_null) pop =
  | Option : ('a : value_or_null). ('a, 'a option) pop
  | Or_null : ('a, 'a or_null) pop
  | Exn : ('a : value_or_null). ('a, 'a) pop

exception Empty

let pop_or_peek_as t ~remove result =
  let[@inline] rec go
    : type (a : value_or_null) (r : value_or_null).
      a t @ local -> (a, r) pop -> a head -> r @ contended portable
    =
    fun t result q ->
    match q with
    | H (Cons cons_r as cons) ->
      if remove
      then (
        match
          Atomic.Loc.compare_and_set
            [%atomic.loc t.inner.head]
            ~if_phys_equal_to:(H cons)
            ~replace_with:cons_r.suffix
        with
        | Set_here ->
          (match result with
           | Option -> Some cons_r.value
           | Or_null -> This cons_r.value
           | Exn -> cons_r.value)
        | Compare_failed ->
          Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
          go t result (Atomic.Loc.get [%atomic.loc t.inner.head]))
      else (
        match result with
        | Option -> Some cons_r.value
        | Or_null -> This cons_r.value
        | Exn -> cons_r.value)
    | H (Head head_r as head) ->
      (match Atomic.Loc.get [%atomic.loc t.inner.tail] with
       | T (Snoc snoc_r as move) ->
         if head_r.counter = snoc_r.counter
         then
           if remove
           then (
             match
               Atomic.Loc.compare_and_set
                 [%atomic.loc t.inner.tail]
                 ~if_phys_equal_to:(T move)
                 ~replace_with:snoc_r.prefix
             with
             | Set_here ->
               (match result with
                | Option -> Some snoc_r.value
                | Or_null -> This snoc_r.value
                | Exn -> snoc_r.value)
             | Compare_failed -> go t result (Atomic.Loc.get [%atomic.loc t.inner.head]))
           else (
             match result with
             | Option -> Some snoc_r.value
             | Or_null -> This snoc_r.value
             | Exn -> snoc_r.value)
         else (
           let (Tail tail_r as tail : (_, [ `Tail ]) queue) =
             Tail { counter = snoc_r.counter; move }
           in
           let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
           if not (phys_equal new_head (H head))
           then go t result new_head
           else (
             match
               Atomic.Loc.compare_and_set
                 [%atomic.loc t.inner.tail]
                 ~if_phys_equal_to:(T move)
                 ~replace_with:(T tail)
             with
             | Set_here ->
               let (Cons cons_r as reversed) = rev move in
               if remove
               then (
                 let after = cons_r.suffix in
                 let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
                 if not (phys_equal new_head (H head))
                 then go t result new_head
                 else (
                   match
                     Atomic.Loc.compare_and_set
                       [%atomic.loc t.inner.head]
                       ~if_phys_equal_to:(H head)
                       ~replace_with:after
                   with
                   | Set_here ->
                     tail_r.move <- Used;
                     (match result with
                      | Option -> Some cons_r.value
                      | Or_null -> This cons_r.value
                      | Exn -> cons_r.value)
                   | Compare_failed ->
                     Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
                     go t result (Atomic.Loc.get [%atomic.loc t.inner.head])))
               else (
                 let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
                 if phys_equal new_head (H head)
                 then (
                   match
                     Atomic.Loc.compare_and_set
                       [%atomic.loc t.inner.head]
                       ~if_phys_equal_to:(H head)
                       ~replace_with:(H reversed)
                   with
                   | Set_here -> tail_r.move <- Used
                   | Compare_failed -> ());
                 (* If head changed or [Compare_failed] someone else completed the move.
                    We did witness the actual head, however, so we can just return
                    it.inner. *)
                 match result with
                 | Option -> Some cons_r.value
                 | Or_null -> This cons_r.value
                 | Exn -> cons_r.value)
             | Compare_failed ->
               Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
               go t result (Atomic.Loc.get [%atomic.loc t.inner.head])))
       | T (Tail tail_r) ->
         (match tail_r.move with
          | Used ->
            let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
            if not (phys_equal new_head (H head))
            then go t result new_head
            else (
              (* Backoff to avoid causing contention via too frequent polling. *)
              Adaptive_backoff.once_unless_alone ~random_key:t.inner.random_key ~log_scale;
              match result with
              | Option -> None
              | Or_null -> Null
              | Exn -> raise Empty)
          | Snoc move_r as move ->
            if head_r.counter < move_r.counter
            then (
              let (Cons cons_r as reversed) = rev move in
              let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
              if not (phys_equal new_head (H head))
              then go t result new_head
              else if remove
              then (
                let after = cons_r.suffix in
                match
                  Atomic.Loc.compare_and_set
                    [%atomic.loc t.inner.head]
                    ~if_phys_equal_to:(H head)
                    ~replace_with:after
                with
                | Set_here ->
                  tail_r.move <- Used;
                  (match result with
                   | Option -> Some cons_r.value
                   | Or_null -> This cons_r.value
                   | Exn -> cons_r.value)
                | Compare_failed ->
                  Adaptive_backoff.once ~random_key:t.inner.random_key ~log_scale;
                  go t result (Atomic.Loc.get [%atomic.loc t.inner.head]))
              else (
                let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
                if phys_equal new_head (H head)
                then (
                  match
                    Atomic.Loc.compare_and_set
                      [%atomic.loc t.inner.head]
                      ~if_phys_equal_to:(H head)
                      ~replace_with:(H reversed)
                  with
                  | Set_here -> tail_r.move <- Used
                  | Compare_failed -> ());
                (* If head changed or [Compare_failed] someone else completed the move. We
                   did witness the actual head, however, so we can just return it.inner. *)
                match result with
                | Option -> Some cons_r.value
                | Or_null -> This cons_r.value
                | Exn -> cons_r.value))
            else (
              let new_head = Atomic.Loc.get [%atomic.loc t.inner.head] in
              if not (phys_equal new_head (H head))
              then go t result new_head
              else (
                (* Backoff to avoid causing contention via too frequent polling. *)
                Adaptive_backoff.once_unless_alone
                  ~random_key:t.inner.random_key
                  ~log_scale;
                match result with
                | Option -> None
                | Or_null -> Null
                | Exn -> raise Empty))))
  in
  let result = go t result (Atomic.Loc.get [%atomic.loc t.inner.head]) in
  (* Magic: elements are pushed at unique and popped or stolen exactly once. *)
  (Obj.magic_unique [@mode portable contended]) result
;;

let[@inline] pop t = pop_or_peek_as t ~remove:true Or_null
let[@inline] pop_opt t = pop_or_peek_as t ~remove:true Option
let[@inline] pop_exn t = pop_or_peek_as t ~remove:true Exn
let[@inline] peek t = pop_or_peek_as t ~remove:false Or_null
let[@inline] peek_opt t = pop_or_peek_as t ~remove:false Option
let[@inline] peek_exn t = pop_or_peek_as t ~remove:false Exn

let[@inline] length t =
  let head = ref (Atomic.Loc.get [%atomic.loc t.inner.head]) in
  let tail = ref (Atomic.Loc.get [%atomic.loc t.inner.tail]) in
  while
    head := Atomic.Loc.get [%atomic.loc t.inner.head];
    tail := Atomic.Loc.get [%atomic.loc t.inner.tail];
    not (phys_equal !head (Atomic.Loc.get [%atomic.loc t.inner.head]))
  do
    ()
  done;
  let head_at =
    match !head with
    | H (Cons r) -> r.counter
    | H (Head r) -> r.counter
  in
  let tail_at =
    match !tail with
    | T (Snoc r) -> r.counter
    | T (Tail r) -> r.counter
  in
  tail_at - head_at + 1
;;

let[@inline] is_empty t = length t = 0
