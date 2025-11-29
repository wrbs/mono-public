open! Core

type 'a t
type ('a, 'acc) fold_f = init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc
type 'a iter_f = f:('a -> unit) @ local -> unit

type ('a, 'acc, 'final) fold_until_f =
  init:'acc
  -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t) @ local
  -> finish:('acc -> 'final) @ local
  -> 'final

type ('a, 'final) iter_until_f =
  f:('a -> (unit, 'final) Continue_or_stop.t) @ local
  -> finish:(unit -> 'final) @ local
  -> 'final

module Uncontrolled (Base : sig
    type 'a t
  end) =
struct
  type ('a, 'res, 'f) state =
    | No_state : ('a, 'b, 'a -> 'b) state
    | Folding : 'acc -> ('a, 'b, 'acc -> 'a -> 'acc * 'b) state
    | Indexed : ('a, 'b, int -> 'a -> 'b) state

  type ('a, 'res, 'b) map_base =
    | Map : ('a, 'b, 'b) map_base
    | Concat_map : ('a, 'b Base.t, 'b) map_base
    | Filter : ('a, bool, 'a) map_base
    | Filter_map : ('a, 'b option, 'b) map_base

  type ('a, 'b) t =
    | Map :
        { kind : ('a, 'res, 'b) map_base
        ; state : ('a, 'res, 'f) state
        ; f : 'f
        }
        -> ('a, 'b) t
    | Cons : 'a -> ('a, 'a) t

  let eval_fold
    : type a b.
      (a, b) t
      -> fold_a:('acc. (a, 'acc) fold_f)
      -> fold_base:('elem 'acc. 'elem Base.t -> ('elem, 'acc) fold_f)
      -> (b, _) fold_f
    =
    fun t ~fold_a ~fold_base ~init ~f ->
    match (t : (a, b) t) with
    | Cons x -> fold_a ~init:(f init x) ~f
    | Map { kind; state; f = map_f } ->
      let res =
        (kind, state, map_f)
        |>
        fun (type res f)
          ((kind : (a, res, b) map_base), (state : (a, res, f) state), (map_f : f)) ->
          let handle_res (input : a) (res : res) acc =
            match kind with
            | Map -> f acc res
            | Concat_map -> fold_base res ~init:acc ~f
            | Filter -> if res then f acc input else acc
            | Filter_map ->
              (match res with
               | Some x -> f acc x
               | None -> acc)
          in
          match state with
          | No_state ->
            fold_a ~init ~f:(fun acc x -> handle_res x (map_f x) acc) [@nontail]
          | Folding fold_init ->
            let acc, _ =
              fold_a ~init:(init, fold_init) ~f:(fun (acc, fold_acc) x ->
                let fold_acc', x' = map_f fold_acc x in
                handle_res x x' acc, fold_acc')
            in
            acc
          | Indexed ->
            let acc, _ =
              fold_a ~init:(init, 0) ~f:(fun (acc, idx) x ->
                handle_res x (map_f idx x) acc, idx + 1)
            in
            acc
      in
      res
  ;;

  let eval_iter
    : type a b.
      (a, b) t
      -> iter_a:a iter_f
      -> iter_base:('elem. 'elem Base.t -> 'elem iter_f)
      -> b iter_f
    =
    fun t ~iter_a ~iter_base ~f ->
    match (t : (a, b) t) with
    | Cons x ->
      f x;
      iter_a ~f
    | Map { kind; state; f = map_f } ->
      let res =
        (kind, state, map_f)
        |>
        fun (type res f)
          ((kind : (a, res, b) map_base), (state : (a, res, f) state), (map_f : f)) ->
          let handle_res (input : a) (res : res) =
            match kind with
            | Map -> f res
            | Concat_map -> iter_base res ~f
            | Filter -> if res then f input else ()
            | Filter_map ->
              (match res with
               | Some x -> f x
               | None -> ())
          in
          match state with
          | No_state -> iter_a ~f:(fun x -> handle_res x (map_f x)) [@nontail]
          | Folding fold_init ->
            let acc = ref fold_init in
            iter_a ~f:(fun x ->
              let acc', x' = map_f !acc x in
              acc := acc';
              handle_res x x')
            [@nontail]
          | Indexed ->
            let idx = ref 0 in
            iter_a ~f:(fun x ->
              let x' = map_f !idx x in
              idx := !idx + 1;
              handle_res x x')
            [@nontail]
      in
      res
  ;;

  let eval_fold_until
    : type a b.
      (a, b) t
      -> fold_until_a:('acc 'final. (a, 'acc, 'final) fold_until_f)
      -> fold_until_base:
           ('elem 'acc 'final. 'elem Base.t -> ('elem, 'acc, 'final) fold_until_f)
      -> (b, _, _) fold_until_f
    =
    fun t ~fold_until_a ~fold_until_base ~init ~f ~finish ->
    match (t : (a, b) t) with
    | Cons x ->
      (match f init x with
       | Continue acc' -> fold_until_a ~init:acc' ~f ~finish
       | Stop result -> result)
    | Map { kind; state; f = map_f } ->
      let res =
        (kind, state, map_f)
        |>
        fun (type res f)
          ((kind : (a, res, b) map_base), (state : (a, res, f) state), (map_f : f)) ->
          let handle_res (input : a) (res : res) acc =
            match kind with
            | Map -> f acc res
            | Concat_map ->
              fold_until_base
                res
                ~init:acc
                ~f:(fun acc x ->
                  match f acc x with
                  | Continue acc' -> Continue acc'
                  | Stop result -> Stop (Continue_or_stop.Stop result))
                ~finish:(fun acc -> Continue acc) [@nontail]
            | Filter -> if res then f acc input else Continue acc
            | Filter_map ->
              (match res with
               | Some x -> f acc x
               | None -> Continue acc)
          in
          match state with
          | No_state ->
            fold_until_a
              ~init
              ~f:(fun acc x -> handle_res x (map_f x) acc)
              ~finish [@nontail]
          | Folding fold_init ->
            fold_until_a
              ~init:(init, fold_init)
              ~f:(fun (acc, fold_acc) x ->
                let fold_acc', x' = map_f fold_acc x in
                match handle_res x x' acc with
                | Continue acc' -> Continue (acc', fold_acc')
                | Stop result -> Stop result)
              ~finish:(fun (acc, _) -> finish acc) [@nontail]
          | Indexed ->
            fold_until_a
              ~init:(init, 0)
              ~f:(fun (acc, idx) x ->
                match handle_res x (map_f idx x) acc with
                | Continue acc' -> Continue (acc', idx + 1)
                | Stop result -> Stop result)
              ~finish:(fun (acc, _) -> finish acc) [@nontail]
      in
      res
  ;;

  let eval_iter_until
    : type a b.
      (a, b) t
      -> iter_until_a:('final. (a, 'final) iter_until_f)
      -> iter_until_base:('elem 'final. 'elem Base.t -> ('elem, 'final) iter_until_f)
      -> (b, _) iter_until_f
    =
    fun t ~iter_until_a ~iter_until_base ~f ~finish ->
    match (t : (a, b) t) with
    | Cons x ->
      (match f x with
       | Continue () -> iter_until_a ~f ~finish
       | Stop result -> result)
    | Map { kind; state; f = map_f } ->
      let res =
        (kind, state, map_f)
        |>
        fun (type res f)
          ((kind : (a, res, b) map_base), (state : (a, res, f) state), (map_f : f)) ->
          let handle_res (input : a) (res : res) =
            match kind with
            | Map -> f res
            | Concat_map ->
              iter_until_base
                res
                ~f:(fun x ->
                  match f x with
                  | Continue () -> Continue ()
                  | Stop result -> Stop (Continue_or_stop.Stop result))
                ~finish:(fun () -> Continue ()) [@nontail]
            | Filter -> if res then f input else Continue ()
            | Filter_map ->
              (match res with
               | Some x -> f x
               | None -> Continue ())
          in
          match state with
          | No_state ->
            iter_until_a ~f:(fun x -> handle_res x (map_f x)) ~finish [@nontail]
          | Folding fold_init ->
            let acc = ref fold_init in
            iter_until_a
              ~f:(fun x ->
                let acc', x' = map_f !acc x in
                acc := acc';
                handle_res x x')
              ~finish [@nontail]
          | Indexed ->
            let idx = ref 0 in
            iter_until_a
              ~f:(fun x ->
                match handle_res x (map_f !idx x) with
                | Continue () ->
                  idx := !idx + 1;
                  Continue ()
                | Stop result -> Stop result)
              ~finish [@nontail]
      in
      res
  ;;
end
