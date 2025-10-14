open! Core

let raise = Stdlib.raise (* core raise isn't unique right now *)

type +'a t =
  | F : (f:('a -> unit) @ local -> unit) -> 'a t
  | Cached : 'a iarray @@ global -> 'a t
  | Cached_rev : 'a iarray @@ global -> 'a t

let iter (t @ local) ~f =
  match t with
  | F iter_f -> iter_f ~f
  | Cached iarray ->
    let n = Iarray.length iarray in
    for idx = 0 to n - 1 do
      let value = Iarray.unsafe_get iarray idx in
      f value
    done
  | Cached_rev iarray ->
    let n = Iarray.length iarray in
    for idx = n - 1 downto 0 do
      let value = Iarray.unsafe_get iarray idx in
      f value
    done
;;

let%template with_stop (f : (('a. unit -> 'a @ portable unique) @ local -> _) @ local) =
  let exception Stop in
  let stop = stack_ fun () -> raise Stop in
  try f stop with
  | Stop -> ()
;;

(* like with_return but without the wrapping record oxcaml doesn't need *)
let with_return (type res) (f : (('a. res -> 'a @ portable unique) @ local -> _) @ local) =
  let exception Return of res in
  let return = stack_ fun x -> raise (Return x) in
  try f return with
  | Return result -> result
;;

let iter_while t ~(f @ local) =
  with_stop (fun stop ->
    iter t ~f:(fun x ->
      match f x with
      | true -> ()
      | false -> stop ())
    [@nontail])
  [@nontail]
;;

let counter () = exclave_
  let count = stack_ (ref 0) in
  stack_
    fun () ->
      let n = !count in
      count := n + 1;
      n
;;

(** Creators *)

[%%template
[@@@alloc.default a @ m = (heap @ global, stack @ local)]

let create (iter @ m) = F iter [@exclave_if_stack a]

let stateful (creator @ m) =
  (create [@alloc a]) (fun ~f ->
    let t = creator () in
    iter t ~f)
  [@exclave_if_stack a]
;;

let repeatedly_call ~f:(next @ m) ~on =
  (create [@alloc a]) (fun ~f ->
    let rec aux x =
      f x;
      aux (next x)
    in
    aux on [@nontail])
  [@exclave_if_stack a]
;;

let unfold state ~f:(next @ m) =
  (create [@alloc a]) (fun ~f ->
    let rec aux state =
      match next state with
      | None -> ()
      | Some (state', x) ->
        f x;
        aux state'
    in
    aux state [@nontail])
  [@exclave_if_stack a]
;;]

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
  create
  @@ fun ~f ->
  let rec local_ loop i =
    let i_to_stop_order = order i stop_i in
    match i_to_stop_order, initial_stride_order with
    | Less, `Less | Greater, `Greater ->
      (* haven't yet reached [stop_i]. Continue. *)
      let next_i = stride i in
      (match order i next_i, initial_stride_order with
       | Equal, _ -> raise_stride_cannot_return_same_value ()
       | Less, `Greater | Greater, `Less ->
         invalid_arg "Iterator.range': stride function cannot change direction"
       | Less, `Less | Greater, `Greater ->
         f i;
         loop next_i)
    | Less, `Greater | Greater, `Less ->
      (* stepped past [stop_i].  Finished. *)
      ()
    | Equal, _ ->
      (* reached [stop_i].  Finished. *)
      (match stop with
       | `inclusive -> f i
       | `exclusive -> ())
  in
  loop start_i [@nontail]
;;

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "Iterator.range: stride must be non-zero";
  range' ~compare ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

let empty = create @@ fun ~f:_ -> ()
let singleton x = create @@ fun ~f -> f x

(* Converison in *)

let of_list x = create @@ fun ~f -> List.iter x ~f
let of_array x = create @@ fun ~f -> Array.iter x ~f
let of_vec x = create @@ fun ~f -> Vec.iter x ~f
let of_queue x = create @@ fun ~f -> Queue.iter x ~f
let of_stack x = create @@ fun ~f -> Stack.iter x ~f
let of_sequence x = create @@ fun ~f -> Sequence.iter x ~f

let of_map x =
  create @@ fun ~f -> Map.iteri x ~f:(fun ~key ~data -> f (key, data)) [@nontail]
;;

let of_map_keys x = create @@ fun ~f -> Map.iter_keys x ~f
let of_map_data x = create @@ fun ~f -> Map.iter x ~f
let of_set x = create @@ fun ~f -> Set.iter x ~f

let of_hashtbl x =
  create @@ fun ~f -> Hashtbl.iteri x ~f:(fun ~key ~data -> f (key, data)) [@nontail]
;;

let of_hashtbl_keys x = create @@ fun ~f -> Hashtbl.iter_keys x ~f
let of_hashtbl_data x = create @@ fun ~f -> Hashtbl.iter x ~f

(* Accumulators *)

let iteri t ~(f @ local) =
  let count = counter () in
  iter t ~f:(fun x -> f (count ()) x) [@nontail]
;;

let length = function
  | Cached x | Cached_rev x -> Iarray.length x
  | F iter ->
    let length = ref 0 in
    iter ~f:(fun _ -> length := !length + 1);
    !length
;;

let fold (t @ local) ~init ~f =
  let acc = ref init in
  iter t ~f:(fun x -> acc := f !acc x);
  !acc
;;

let find_map t ~(f @ local) =
  with_return (fun return ->
    iter t ~f:(fun x ->
      match f x with
      | Some y -> return (Some y)
      | None -> ());
    None)
  [@nontail]
;;

let find t ~(f @ local) = find_map t ~f:(fun x -> Option.some_if (f x) x) [@nontail]

let find_mapi t ~(f @ local) =
  let count = counter () in
  with_return (fun return ->
    iter t ~f:(fun x ->
      match f (count ()) x with
      | Some y -> return (Some y)
      | None -> ());
    None)
  [@nontail]
;;

let findi t ~(f @ local) =
  find_mapi t ~f:(fun idx x -> Option.some_if (f idx x) x) [@nontail]
;;

let exists t ~(f @ local) =
  with_return (fun return ->
    iter t ~f:(fun x ->
      match f x with
      | true -> return true
      | false -> ());
    false)
  [@nontail]
;;

let mem t x ~(equal @ local) = exists t ~f:(fun x' -> equal x x') [@nontail]
let for_all t ~(f @ local) = not (exists t ~f:(fun x -> not (f x)))

let max_elt t ~(compare @ local) =
  fold t ~init:None ~f:(fun acc x ->
    match acc with
    | None -> Some x
    | Some prev ->
      (match compare x prev > 0 with
       | true -> Some x
       | false -> Some prev))
  [@nontail]
;;

let min_elt t ~(compare @ local) =
  fold t ~init:None ~f:(fun acc x ->
    match acc with
    | None -> Some x
    | Some prev ->
      (match compare x prev < 0 with
       | true -> Some x
       | false -> Some prev))
  [@nontail]
;;

let sum (type sum) (module M : Container.Summable with type t = sum) t ~(f @ local) =
  fold t ~init:M.zero ~f:(fun acc x -> M.( + ) acc (f x)) [@nontail]
;;

(* Conversion out *)

let to_list t = fold t ~init:[] ~f:(fun l x -> x :: l) |> List.rev
let add_to_vec t ~vec = iter t ~f:(fun x -> Vec.push_back vec x)

let to_vec ?size_hint t =
  match t with
  | Cached iarray -> iarray |> Iarray.to_array |> Vec.of_array
  | Cached_rev iarray ->
    let vec = Vec.create ~initial_capacity:(Iarray.length iarray) () in
    add_to_vec t ~vec;
    vec
  | F _ ->
    let vec = Vec.create ?initial_capacity:size_hint () in
    add_to_vec t ~vec;
    vec
;;

let add_to_queue t ~queue = iter t ~f:(fun x -> Queue.enqueue queue x)

let to_queue ?size_hint t =
  let queue = Queue.create ?capacity:size_hint () in
  add_to_queue t ~queue;
  queue
;;

let add_to_stack t ~stack = iter t ~f:(fun x -> Stack.push stack x)

let to_stack t =
  let stack = Stack.create () in
  add_to_stack t ~stack;
  stack
;;

let to_iarray ?size_hint t =
  match t with
  | Cached iarray -> iarray
  | Cached_rev iarray -> Iarray.rev iarray
  | _ -> to_vec ?size_hint t |> Vec.to_iarray
;;

let to_array ?size_hint t = Iarray.to_array (to_iarray ?size_hint t)
let to_set cmp t = fold t ~init:(Set.empty cmp) ~f:(fun set x -> Set.add set x)
let add_to_hash_set t ~set = iter t ~f:(fun x -> Hash_set.add set x)

let to_hash_set ?size key t =
  let set = Hash_set.create ?size key in
  add_to_hash_set t ~set;
  set
;;

let to_map cmp t =
  Map.of_iteri cmp ~iteri:(fun ~f ->
    iter t ~f:(fun (key, data) -> f ~key ~data) [@nontail])
;;

let to_map_exn cmp t =
  Map.of_iteri_exn cmp ~iteri:(fun ~f ->
    iter t ~f:(fun (key, data) -> f ~key ~data) [@nontail])
;;

let to_hashtbl_reduce key_m t ~reduce =
  let table = Hashtbl.create key_m in
  iter t ~f:(fun (key, data) ->
    Hashtbl.update table key ~f:(function
      | None -> data
      | Some prev -> reduce key prev data));
  table
;;

let to_hashtbl_exn key_m t =
  let table = Hashtbl.create key_m in
  iter t ~f:(fun (key, data) -> Hashtbl.add_exn table ~key ~data);
  table
;;

(* Caching *)

let ensure_cached ?size_hint (t @ local) =
  match t with
  | Cached x -> x, ~rev:false
  | Cached_rev x -> x, ~rev:true
  | F _ ->
    let vec = to_vec ?size_hint t in
    Vec.to_iarray vec, ~rev:false
;;

let create_cached x ~rev =
  match rev with
  | false -> Cached x
  | true -> Cached_rev x
;;

let cache ?size_hint (t @ local) =
  let x, ~rev = ensure_cached ?size_hint t in
  create_cached x ~rev
;;

let cache_with_length ?size_hint (t @ local) =
  let x, ~rev = ensure_cached ?size_hint t in
  create_cached x ~rev, ~length:(Iarray.length x)
;;

let of_iarray x = Cached x
let of_array_cached x = of_iarray (Iarray.of_array x)

let unsafe_of_array ~promise_no_mutation:x =
  of_iarray (Iarray.unsafe_of_array__promise_no_mutation x)
;;

let of_vec_cached x = of_iarray (Vec.to_iarray x)
let of_queue_cached x = unsafe_of_array ~promise_no_mutation:(Queue.to_array x)
let of_stack_cached x = unsafe_of_array ~promise_no_mutation:(Stack.to_array x)

let rev ?size_hint (t @ local) =
  let x, ~rev = ensure_cached ?size_hint t in
  create_cached x ~rev:(not rev)
;;

let sort ?size_hint t ~compare =
  let vec =
    match t with
    | Cached x | Cached_rev x -> Vec.of_array (Iarray.to_array x)
    | F _ -> to_vec ?size_hint t
  in
  Vec.sort vec ~compare;
  Cached (Vec.to_iarray vec)
;;

let to_sequence ?size_hint t =
  let arr, ~rev = ensure_cached ?size_hint t in
  let n = Iarray.length arr in
  match rev with
  | false ->
    Sequence.unfold ~init:0 ~f:(fun idx ->
      if idx >= n then None else Some (arr.:(idx), idx + 1))
  | true ->
    Sequence.unfold ~init:(n - 1) ~f:(fun idx ->
      if idx < 0 then None else Some (arr.:(idx), idx - 1))
;;

[%%template
[@@@alloc.default a @ m = (heap @ global, stack @ local)]

let forever (t @ m) =
  (create [@alloc a]) (fun ~f ->
    while true do
      iter t ~f
    done)
  [@exclave_if_stack a]
;;

let map (t @ m) ~f:(map_f @ m) =
  (create [@alloc a]) (fun ~f -> iter t ~f:(fun x -> f (map_f x)) [@nontail])
  [@exclave_if_stack a]
;;

let mapi t ~f:map_f =
  (create [@alloc a]) (fun ~f ->
    let count = counter () in
    iter t ~f:(fun x -> f (map_f (count ()) x)) [@nontail])
  [@exclave_if_stack a]
;;

let filter_map t ~f:filter_f =
  (create [@alloc a]) (fun ~f ->
    iter t ~f:(fun x ->
      match filter_f x with
      | None -> ()
      | Some y -> f y)
    [@nontail])
  [@exclave_if_stack a]
;;

let filter_mapi t ~f:filter_f =
  (create [@alloc a]) (fun ~f ->
    let count = counter () in
    iter t ~f:(fun x ->
      match filter_f (count ()) x with
      | None -> ()
      | Some y -> f y)
    [@nontail])
  [@exclave_if_stack a]
;;

let filter t ~f =
  (filter_map [@alloc a]) t ~f:(fun x -> Option.some_if (f x) x) [@exclave_if_stack a]
;;

let filteri t ~f =
  (filter_mapi [@alloc a]) t ~f:(fun idx x -> Option.some_if (f idx x) x)
  [@exclave_if_stack a]
;;

let filter_opt t = (filter_map [@alloc a]) t ~f:Fn.id [@exclave_if_stack a]

let folding_map t ~init ~f:map_f =
  (create [@alloc a]) (fun ~f ->
    let acc = ref init in
    iter t ~f:(fun x ->
      let acc', y = map_f !acc x in
      acc := acc';
      f y)
    [@nontail])
  [@exclave_if_stack a]
;;

let folding_filter_map t ~init ~f:map_f =
  (create [@alloc a]) (fun ~f ->
    let acc = ref init in
    iter t ~f:(fun x ->
      let acc', y = map_f !acc x in
      acc := acc';
      Option.iter y ~f)
    [@nontail])
  [@exclave_if_stack a]
;;

let enumerated t = (mapi [@alloc a]) t ~f:(fun idx x -> idx, x) [@exclave_if_stack a]

let concat_map t ~f:bind_f =
  (create [@alloc a]) (fun ~f -> iter t ~f:(fun x -> iter (bind_f x) ~f) [@nontail])
  [@exclave_if_stack a]
;;

let concat_mapi t ~f:bind_f =
  (create [@alloc a]) (fun ~f ->
    let count = counter () in
    iter t ~f:(fun x -> iter (bind_f (count ()) x) ~f) [@nontail])
  [@exclave_if_stack a]
;;

let concat_map_list t ~f =
  (concat_map [@alloc a]) t ~f:(fun x -> of_list (f x)) [@exclave_if_stack a]
;;

let concat_map_listi t ~f =
  (concat_mapi [@alloc a]) t ~f:(fun idx x -> of_list (f idx x)) [@exclave_if_stack a]
;;

let concat ts =
  (create [@alloc a]) (fun ~f -> iter ts ~f:(fun t -> iter t ~f) [@nontail])
  [@exclave_if_stack a]
;;

let concat_list (ts @ m) =
  (create [@alloc a]) (fun ~f ->
    (* List.iter isn't local, but we need to convince it's fine here*)
    let rec aux = function
      | [] -> ()
      | x :: xs ->
        iter x ~f;
        aux xs
    in
    aux ts [@nontail])
  [@exclave_if_stack a]
;;

let cons x t =
  (create [@alloc a]) (fun ~f ->
    f x;
    iter t ~f)
  [@exclave_if_stack a]
;;

let append t t' =
  (create [@alloc a]) (fun ~f ->
    iter t ~f;
    iter t' ~f)
  [@exclave_if_stack a]
;;

let take_while t ~f:pred_f =
  (create [@alloc a]) (fun ~f ->
    with_stop (fun stop ->
      iter t ~f:(fun x ->
        match pred_f x with
        | false -> stop ()
        | true -> f x)
      [@nontail])
    [@nontail])
  [@exclave_if_stack a]
;;

let drop_while t ~f:pred_f =
  (create [@alloc a]) (fun ~f ->
    let dropping = ref true in
    iter t ~f:(fun x ->
      match !dropping with
      | false -> f x
      | true ->
        (match pred_f x with
         | true -> ()
         | false ->
           dropping := false;
           f x))
    [@nontail])
  [@exclave_if_stack a]
;;

let take t ~n =
  (create [@alloc a]) (fun ~f ->
    with_stop (fun stop ->
      iteri t ~f:(fun idx x ->
        match idx >= n with
        | true -> stop ()
        | false -> f x)
      [@nontail])
    [@nontail])
  [@exclave_if_stack a]
;;

let drop t ~n =
  (create [@alloc a]) (fun ~f ->
    let count = ref 0 in
    iter t ~f:(fun x ->
      match !count >= n with
      | true -> f x
      | false -> count := !count + 1)
    [@nontail])
  [@exclave_if_stack a]
;;

let group' t ~break ~handle_vec =
  (create [@alloc a]) (fun ~f ->
    let vec = Vec.create () in
    let flush_vec () =
      let result = handle_vec vec in
      Vec.clear vec;
      f result
    in
    let last = ref None in
    iter t ~f:(fun cur_x ->
      Option.iter !last ~f:(fun last_x -> if break last_x cur_x then flush_vec ());
      Vec.push_back vec cur_x;
      last := Some cur_x);
    if Vec.length vec <> 0 then flush_vec () [@nontail])
  [@exclave_if_stack a]
;;

let group t ~break =
  (group' [@alloc a]) t ~break ~handle_vec:(fun vec -> Cached (Vec.to_iarray vec))
  [@exclave_if_stack a]
;;

let group_map t ~break ~f =
  (group' [@alloc a]) t ~break ~handle_vec:(fun vec -> f (local_ of_vec vec) [@nontail])
  [@exclave_if_stack a]
;;

let chunks_of' t ~length ~handle_vec =
  if length <= 0
  then invalid_argf "List.chunks_of: invalid length, expected <= but got %d" length ();
  (create [@alloc a]) (fun ~f ->
    let vec = Vec.create ~initial_capacity:length () in
    let flush_vec () =
      let result = handle_vec vec in
      Vec.clear vec;
      f result
    in
    iter t ~f:(fun x ->
      Vec.push_back vec x;
      if Vec.length vec = length then flush_vec ());
    if Vec.length vec <> 0 then flush_vec () [@nontail])
  [@exclave_if_stack a]
;;

let chunks_of t ~length =
  (chunks_of' [@alloc a]) t ~length ~handle_vec:(fun vec -> Cached (Vec.to_iarray vec))
  [@exclave_if_stack a]
;;

let chunks_of_map t ~length ~f =
  (chunks_of' [@alloc a]) t ~length ~handle_vec:(fun vec ->
    f (local_ of_vec vec) [@nontail])
  [@exclave_if_stack a]
;;]

let fill_array t ~(array @ local) =
  iteri
    ((take [@alloc stack]) t ~n:(Array.length array))
    ~f:(fun idx value -> array.(idx) <- value) [@nontail]
;;

(* For monad *)

module M : Monad.S with type 'a t := 'a t = struct
  type nonrec 'a t = 'a t

  let return = singleton
  let bind = concat_map
  let map = `Custom map

  include functor Monad.Make

  let join = concat
end

include M

module Using_effects = struct
  module Unique_driver = struct
    type (_, _) effect = Yield : 'a -> (unit, 'a) effect

    module E = Effect.Make1 (struct
        type ('a, 'p) t = ('a, 'p) effect
      end)

    type ('a, 'es) t = (unit, unit, 'a, 'es) E.Continuation.t

    type ('a, 'es) step =
      | Next of 'a @@ aliased global * ('a, 'es) t
      | Done

    let create t =
      E.fiber (fun handler () ->
        iter t ~f:(fun x -> E.perform handler (Yield x)) [@nontail])
    ;;

    let create_with length f =
      E.fiber_with length (fun (handler :: handlers) () ->
        let t = f handlers in
        iter t ~f:(fun x -> E.perform handler (Yield x)) [@nontail])
    ;;

    let step (t : _ t) @ unique =
      (match Effect.continue t () [] with
       | Value () -> Done
       | Exception exn -> raise exn
       | Operation (Yield x, t') -> Next (x, t'))
    ;;

    let step_with (t : _ t) handlers @ unique =
      (match Effect.continue t () handlers with
       | Value () -> Done
       | Exception exn -> raise exn
       | Operation (Yield x, t') -> Next (x, t'))
    ;;
  end

  module Driver = struct
    type 'a t =
      | T :
          { ref : ('a, 'es) Unique_driver.t option Unique.Ref.t
          ; handlers : 'es Effect.Handler.List.t
          }
          -> 'a t

    let create iter =
      let driver = Unique_driver.create iter in
      T { ref = Unique.Ref.make (Some driver); handlers = [] }
    ;;

    let create_with handlers f =
      let driver = Unique_driver.create_with (Effect.Handler.List.length handlers) f in
      exclave_ T { ref = Unique.Ref.make (Some driver); handlers }
    ;;

    let next t =
      let (T { ref; handlers }) = t in
      match Unique.Ref.exchange ref None with
      | None -> None
      | Some driver' ->
        (match Unique_driver.step_with driver' handlers with
         | Done -> None
         | Next (value, driver') ->
           let (_ : _ Unique_driver.t option) = Unique.Ref.exchange ref (Some driver') in
           Some value)
    ;;

    let%template to_iter (t @ m) =
      F
        (fun ~f ->
          let rec loop () =
            match next t with
            | None -> ()
            | Some x ->
              f x;
              loop ()
          in
          loop () [@nontail])
      [@exclave_if_stack a]
    [@@alloc a @ m = (heap @ global, stack @ local)]
    ;;
  end

  [%%template
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  let map2 (ta @ m) tb ~f:(map_f @ m) @ m =
    (create [@alloc a]) (fun ~f ->
      with_stop (fun stop ->
        let driver_b = Driver.create tb in
        iter ta ~f:(fun a ->
          match Driver.next driver_b with
          | None -> stop ()
          | Some b -> f (map_f a b))
        [@nontail])
      [@nontail])
    [@exclave_if_stack a]
  ;;

  let zip t t' = (map2 [@alloc a]) t t' ~f:(fun a b -> a, b) [@exclave_if_stack a]

  let zip_full (ta @ m) tb @ m =
    (create [@alloc a]) (fun ~f ->
      let driver_b = Driver.create tb in
      iter ta ~f:(fun a ->
        match Driver.next driver_b with
        | None -> f (`Left a)
        | Some b -> f (`Both (a, b)));
      let rec aux () =
        match Driver.next driver_b with
        | None -> ()
        | Some b ->
          f (`Right b);
          aux ()
      in
      aux () [@nontail])
    [@exclave_if_stack a]
  ;;

  let mapi2 (ta @ m) tb ~f:(map_f @ m) @ m =
    (create [@alloc a]) (fun ~f ->
      with_stop (fun stop ->
        let count = counter () in
        let driver_b = Driver.create tb in
        iter ta ~f:(fun a ->
          match Driver.next driver_b with
          | None -> stop ()
          | Some b -> f (map_f (count ()) a b))
        [@nontail])
      [@nontail])
    [@exclave_if_stack a]
  ;;

  let map3 (ta @ m) tb tc ~f:(map_f @ m) @ m =
    (create [@alloc a]) (fun ~f ->
      with_stop (fun stop ->
        let driver_b = Driver.create tb in
        let driver_c = Driver.create tc in
        iter ta ~f:(fun a ->
          match Option.both (Driver.next driver_b) (Driver.next driver_c) with
          | None -> stop ()
          | Some (b, c) -> f (map_f a b c))
        [@nontail])
      [@nontail])
    [@exclave_if_stack a]
  ;;

  let mapi3 (ta @ m) tb tc ~f:(map_f @ m) @ m =
    (create [@alloc a]) (fun ~f ->
      with_stop (fun stop ->
        let count = counter () in
        let driver_b = Driver.create tb in
        let driver_c = Driver.create tc in
        iter ta ~f:(fun a ->
          match Option.both (Driver.next driver_b) (Driver.next driver_c) with
          | None -> stop ()
          | Some (b, c) -> f (map_f (count ()) a b c))
        [@nontail])
      [@nontail])
    [@exclave_if_stack a]
  ;;]

  let round_robin ts =
    create
    @@ fun ~f ->
    let drivers = map ts ~f:(fun t -> Driver.create t) |> to_vec in
    while not (Vec.is_empty drivers) do
      Vec.Inplace.filter drivers ~f:(fun driver ->
        match Driver.next driver with
        | Some x ->
          f x;
          true
        | None -> false)
    done
  ;;
end
