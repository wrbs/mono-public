open! Core

type ('a, 'b) continue_or_stop = ('a, 'b) Continue_or_stop.t =
  | Continue of 'a
  | Stop of 'b

let map_cos c ~f =
  match c with
  | Continue x -> Continue (f x)
  | Stop x -> Stop x
;;

type ('a, 'acc) fold_f = init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc

type ('a, 'acc, 'final) fold_until_f =
  init:'acc
  -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t) @ local
  -> finish:('acc -> 'final) @ local
  -> 'final

type 'a fold = { fold : 'acc. ('a, 'acc) fold_f } [@@unboxed]

type +'a t =
  { fold' : 'a fold or_null (* null when it's definitely infinite *)
  ; fold_until : 'acc 'final. ('a, 'acc, 'final) fold_until_f
  }

let is_definitely_infinite t = Or_null.is_null t.fold'

let create
  ~(fold : 'acc. ('a, 'acc) fold_f)
  ~(fold_until : 'acc 'final. ('a, 'acc, 'final) fold_until_f)
  =
  { fold' = This { fold }; fold_until }
;;

let create_of_fold_until (fold_until : 'acc 'final. ('a, 'acc, 'final) fold_until_f) ~fold
  =
  let fold' =
    match fold with
    | `pointless_because_infinite -> Null
    | `derive_it_for_me ->
      This
        { fold =
            (fun ~init ~f ->
              fold_until
                ~init
                ~f:(fun acc x -> Continue (f acc x))
                ~finish:(fun acc -> acc) [@nontail])
        }
  in
  { fold'; fold_until }
;;

let create_of_fold_with_exceptions__slow_on_web
  (fold : 'acc. init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc)
  =
  create
    ~fold
    ~fold_until:(fun (type stop) ~init ~f ~finish ->
      let exception Stop of stop in
      match
        fold ~init ~f:(fun acc x ->
          match f acc x with
          | Continue acc' -> acc'
          | Stop result -> raise (Stop result))
      with
      | x -> finish x
      | exception Stop result -> result)
;;

let fold_until t ~init ~f ~finish = t.fold_until ~init ~f ~finish

let fold t ~init ~f =
  match t.fold' with
  | This x -> x.fold ~init ~f
  | Null ->
    t.fold_until
      ~init
      ~f:(fun acc x -> Continue (f acc x))
      ~finish:(fun acc -> acc) [@nontail]
;;

include struct
  open Container.Make (struct
      type nonrec 'a t = 'a t

      let fold_until = fold_until
      let fold = `Custom fold
      let iter_until = `Define_using_fold_until
      let iter = `Define_using_fold
      let length = `Define_using_fold
    end)

  let length = length
  let is_empty = is_empty
  let iter = iter
  let iter_until = iter_until
  let fold = fold
  let fold_result = fold_result
  let mem = mem
  let exists = exists
  let for_all = for_all
  let count = count
  let sum = sum
  let find = find
  let find_map = find_map
  let min_elt = min_elt
  let max_elt = max_elt
  let to_list = to_list
  let to_array = to_array
end

let empty =
  create
    ~fold:(fun ~init ~f:_ -> init)
    ~fold_until:(fun ~init ~f:_ ~finish -> finish init)
;;

let singleton x =
  create
    ~fold:(fun ~init ~f -> f init x)
    ~fold_until:(fun ~init ~f ~finish ->
      match f init x with
      | Continue acc -> finish acc
      | Stop result -> result)
;;

let unfold state ~f:f_unfold =
  create
    ~fold:(fun ~init ~f ->
      let rec loop ~state ~acc =
        match f_unfold state with
        | None -> acc
        | Some (state', x) -> loop ~state:state' ~acc:(f acc x)
      in
      loop ~state ~acc:init [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      let rec loop ~state ~acc =
        match f_unfold state with
        | None -> finish acc
        | Some (state', x) ->
          (match f acc x with
           | Continue acc -> loop ~state:state' ~acc
           | Stop result -> result)
      in
      loop ~state ~acc:init [@nontail])
;;

let repeatedly_call ~f:next ~on =
  create_of_fold_until ~fold:`pointless_because_infinite (fun ~init ~f ~finish:_ ->
    let rec loop ~state ~acc =
      match f acc state with
      | Stop result -> result
      | Continue acc' -> loop ~state:(next state) ~acc:acc'
    in
    loop ~state:on ~acc:init [@nontail])
;;

let range' ~compare ~stride ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  let next_i = stride start_i in
  let order x y = Ordering.of_int (compare x y) in
  let raise_stride_cannot_return_same_value () =
    invalid_arg "Iterator.range': stride function cannot return the same value"
  in
  let initial_stride_order =
    match order start_i next_i with
    | Equal -> raise_stride_cannot_return_same_value ()
    | Less -> `Less
    | Greater -> `Greater
  in
  let start_i =
    match start with
    | `inclusive -> start_i
    | `exclusive -> next_i
  in
  (* function sufficiently complicated overhead of fold seems not worthwhile *)
  create_of_fold_until ~fold:`derive_it_for_me (fun ~init ~f ~finish ->
    let rec loop ~i ~acc =
      let i_to_stop_order = order i stop_i in
      match i_to_stop_order, initial_stride_order with
      | Less, `Less | Greater, `Greater ->
        (* haven't yet reached [stop_i]. Continue. *)
        let next_i = stride i in
        (match order i next_i, initial_stride_order with
         | Equal, _ -> raise_stride_cannot_return_same_value ()
         | Less, `Greater | Greater, `Less ->
           invalid_arg "Collection.range': stride function cannot change direction"
         | Less, `Less | Greater, `Greater ->
           (match f acc i with
            | Continue acc -> loop ~i:next_i ~acc
            | Stop result -> result))
      | Less, `Greater | Greater, `Less ->
        (* stepped past [stop_i].  Finished. *)
        finish acc
      | Equal, _ ->
        (* reached [stop_i].  Finished. *)
        (match stop with
         | `inclusive ->
           (match f acc i with
            | Continue acc -> finish acc
            | Stop result -> result)
         | `exclusive -> finish acc)
    in
    loop ~i:start_i ~acc:init [@nontail])
;;

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "Collection.range: stride must be non-zero";
  range' ~compare ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

let list x =
  create
    ~fold:(fun ~init ~f -> List.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> List.fold_until x ~init ~f ~finish)
;;

let sequence x =
  create
    ~fold:(fun ~init ~f -> Sequence.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Sequence.fold_until x ~init ~f ~finish)
;;

let set x =
  create
    ~fold:(fun ~init ~f -> Set.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Set.fold_until x ~init ~f ~finish)
;;

let vec x =
  create
    ~fold:(fun ~init ~f -> Vec.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Vec.fold_until x ~init ~f ~finish)
;;

let array x =
  create
    ~fold:(fun ~init ~f -> Array.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Array.fold_until x ~init ~f ~finish)
;;

let iarray x =
  create
    ~fold:(fun ~init ~f -> Iarray.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Iarray.fold_until x ~init ~f ~finish)
;;

let queue x =
  create
    ~fold:(fun ~init ~f -> Queue.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Queue.fold_until x ~init ~f ~finish)
;;

let stack x =
  create
    ~fold:(fun ~init ~f -> Stack.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Stack.fold_until x ~init ~f ~finish)
;;

let hash_set x =
  create
    ~fold:(fun ~init ~f -> Hash_set.fold x ~init ~f)
    ~fold_until:(fun ~init ~f ~finish -> Hash_set.fold_until x ~init ~f ~finish)
;;

let map_entries x =
  create
    ~fold:(fun ~init ~f ->
      Map.fold x ~init ~f:(fun ~key ~data x -> f x (key, data)) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      Map.fold_until x ~init ~f:(fun ~key ~data x -> f x (key, data)) ~finish [@nontail])
;;

let map_keys x =
  create
    ~fold:(fun ~init ~f -> Map.fold x ~init ~f:(fun ~key ~data:_ x -> f x key) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      Map.fold_until x ~init ~f:(fun ~key ~data:_ x -> f x key) ~finish [@nontail])
;;

let map_data x =
  create
    ~fold:(fun ~init ~f ->
      Map.fold x ~init ~f:(fun ~key:_ ~data x -> f x data) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      Map.fold_until x ~init ~f:(fun ~key:_ ~data x -> f x data) ~finish [@nontail])
;;

let hashtbl_entries x =
  create_of_fold_with_exceptions__slow_on_web (fun ~init ~f ->
    Hashtbl.fold x ~init ~f:(fun ~key ~data x -> f x (key, data)) [@nontail])
;;

let hashtbl_keys x =
  create_of_fold_with_exceptions__slow_on_web (fun ~init ~f ->
    Hashtbl.fold x ~init ~f:(fun ~key ~data:_ x -> f x key) [@nontail])
;;

let hashtbl_data x =
  create_of_fold_with_exceptions__slow_on_web (fun ~init ~f ->
    Hashtbl.fold x ~init ~f:(fun ~key:_ ~data x -> f x data) [@nontail])
;;

let forever t =
  create_of_fold_until ~fold:`pointless_because_infinite (fun ~init ~f ~finish:_ ->
    let rec loop acc =
      t.fold_until ~init:acc ~f ~finish:(fun acc -> loop acc) [@nontail]
    in
    loop init [@nontail])
;;

let create_mapping_fold
  :  'a t -> fold:('acc. ('inner_acc. ('a, 'inner_acc) fold_f) -> ('b, 'acc) fold_f)
  -> fold_until:('acc 'final. ('b, 'acc, 'final) fold_until_f) -> 'b t
  =
  fun t ~fold ~fold_until ->
  let fold' =
    match t.fold' with
    | Null -> Null
    | This folder -> This { fold = (fun ~init ~f -> fold folder.fold ~init ~f) }
  in
  { fold'; fold_until }
;;

let enumerated t =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f ->
      let _, acc =
        fold ~init:(0, init) ~f:(fun (count, acc) x -> count + 1, f acc (count, x))
      in
      acc)
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until
        ~init:(0, init)
        ~f:(fun (count, acc) x ->
          f acc (count, x) |> map_cos ~f:(fun acc' -> count + 1, acc'))
        ~finish:(fun (_, acc) -> finish acc) [@nontail])
;;

let cons x t =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f -> fold ~init:(f init x) ~f)
    ~fold_until:(fun ~init ~f ~finish ->
      match f init x with
      | Continue acc -> t.fold_until ~init:acc ~f ~finish
      | Stop result -> result)
;;

let append t t' =
  create
    ~fold:(fun ~init ~f -> fold t' ~init:(fold t ~init ~f) ~f)
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until ~init ~f ~finish:(fun acc -> t'.fold_until ~init:acc ~f ~finish)
      [@nontail])
;;

let concat ts =
  create
    ~fold:(fun ~init ~f -> fold ts ~init ~f:(fun acc t -> fold t ~init:acc ~f) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      ts.fold_until
        ~init
        ~f:(fun acc t ->
          t.fold_until
            ~init:acc
            ~f:(fun acc x ->
              match f acc x with
              | Continue acc' -> Continue acc'
              | Stop result -> Stop (Stop result))
            ~finish:(fun acc -> Continue acc) [@nontail])
        ~finish [@nontail])
;;

let map t ~f:map_f =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f -> fold ~init ~f:(fun acc x -> f acc (map_f x)) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until ~init ~f:(fun acc x -> f acc (map_f x)) ~finish [@nontail])
;;

let mapi t ~f = map (enumerated t) ~f:(fun (idx, x) -> f idx x)
let concat_map t ~f = map t ~f |> concat
let concat_mapi t ~f = mapi t ~f |> concat

let filter_map t ~f:filter_f =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f ->
      fold ~init ~f:(fun acc x ->
        match filter_f x with
        | Some x' -> f acc x'
        | None -> acc)
      [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until
        ~init
        ~f:(fun acc x ->
          match filter_f x with
          | Some x' -> f acc x'
          | None -> Continue acc)
        ~finish [@nontail])
;;

let filter_mapi t ~f = filter_map (enumerated t) ~f:(fun (idx, x) -> f idx x)
let filter t ~f = filter_map t ~f:(fun x -> Option.some_if (f x) x)
let filteri t ~f = filter_mapi t ~f:(fun idx x -> Option.some_if (f idx x) x)

let folding_map t ~init:outer_init ~f:outer_f =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f ->
      let _, acc =
        fold ~init:(outer_init, init) ~f:(fun (outer_acc, acc) x ->
          let outer_acc', x' = outer_f outer_acc x in
          let acc' = f acc x' in
          outer_acc', acc')
      in
      acc)
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until
        ~init:(outer_init, init)
        ~f:(fun (outer_acc, acc) x ->
          let outer_acc', x' = outer_f outer_acc x in
          f acc x' |> map_cos ~f:(fun acc' -> outer_acc', acc'))
        ~finish:(fun (_, acc) -> finish acc) [@nontail])
;;

let folding_filter_map t ~init:outer_init ~f:outer_f =
  create_mapping_fold
    t
    ~fold:(fun fold ~init ~f ->
      let _, acc =
        fold ~init:(outer_init, init) ~f:(fun (outer_acc, acc) x ->
          let outer_acc', x_opt = outer_f outer_acc x in
          match x_opt with
          | None -> outer_acc', acc
          | Some x' ->
            let acc' = f acc x' in
            outer_acc', acc')
      in
      acc)
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until
        ~init:(outer_init, init)
        ~f:(fun (outer_acc, acc) x ->
          let outer_acc', x_opt = outer_f outer_acc x in
          match x_opt with
          | None -> Continue (outer_acc', acc)
          | Some x' -> f acc x' |> map_cos ~f:(fun acc' -> outer_acc', acc'))
        ~finish:(fun (_, acc) -> finish acc) [@nontail])
;;

let filter_opt t = filter_map t ~f:Fn.id

let folding_take_while t ~init:outer_init ~f:outer_f =
  (* Both uses [fold_until] as we need to stop once done *)
  create
    ~fold:(fun ~init ~f ->
      t.fold_until
        ~init:(outer_init, init)
        ~f:(fun (outer_acc, acc) x ->
          match outer_f outer_acc x with
          | None -> Stop acc
          | Some outer_acc' -> Continue (outer_acc', f acc x))
        ~finish:(fun (_, acc) -> acc) [@nontail])
    ~fold_until:(fun ~init ~f ~finish ->
      t.fold_until
        ~init:(outer_init, init)
        ~f:(fun (outer_acc, acc) x ->
          match outer_f outer_acc x with
          | None -> Stop (finish acc)
          | Some outer_acc' -> f acc x |> map_cos ~f:(fun acc' -> outer_acc', acc'))
        ~finish:(fun (_, acc) -> finish acc) [@nontail])
;;

let take_while t ~f =
  folding_take_while t ~init:() ~f:(fun () x -> Option.some_if (f x) ())
;;

let drop_while t ~f =
  folding_filter_map t ~init:`dropping ~f:(fun state x ->
    match state with
    | `dropping ->
      (match f x with
       | true -> `dropping, None
       | false -> `taking, Some x)
    | `taking -> `taking, Some x)
;;

let take t ~n =
  folding_take_while t ~init:n ~f:(fun k _ ->
    match k > 0 with
    | true -> Some (k - 1)
    | false -> None)
;;

let drop t ~n =
  folding_filter_map t ~init:(`dropping n) ~f:(fun state x ->
    match state with
    | `dropping k ->
      (match k > 0 with
       | true -> `dropping (k - 1), None
       | false -> `taking, Some x)
    | `taking -> `taking, Some x)
;;
