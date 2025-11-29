open! Core

type (-'a, +'b) t =
  | T :
      { acc : 'acc
      ; add : 'acc -> 'a -> 'acc
      ; finish : 'acc -> 'b
      }
      -> ('a, 'b) t

module Output = struct
  module A = struct
    include Applicative.Make2 (struct
        type nonrec ('result, 'elem) t = ('elem, 'result) t

        let return x = T { acc = (); add = (fun () _ -> ()); finish = (fun () -> x) }

        let apply t_f t_x =
          let (T t_f) = t_f in
          let (T t_x) = t_x in
          T
            { acc = t_f.acc, t_x.acc
            ; add = (fun (acc_f, acc_x) v -> t_f.add acc_f v, t_x.add acc_x v)
            ; finish =
                (fun (acc_f, acc_x) ->
                  let f = t_f.finish acc_f in
                  let x = t_x.finish acc_x in
                  f x)
            }
        ;;

        let map =
          `Custom
            (fun t ~f ->
              let (T { acc; add; finish }) = t in
              T { acc; add; finish = (fun x -> f (finish x)) })
        ;;
      end)
  end

  include A

  module Let_syntax = struct
    let return = return

    include (
      A :
        Applicative.Applicative_infix2 with type ('result, 'elem) t := ('elem, 'result) t)

    module Let_syntax = struct
      let return = return
      let map = map
      let both = both
    end
  end
end

module Input = struct
  let map t ~f =
    let (T { acc; add; finish }) = t in
    T { acc; add = (fun acc x -> add acc (f x)); finish }
  ;;

  let ( @-> ) f t = map t ~f

  let mapi t ~f =
    let (T { acc; add; finish }) = t in
    T
      { acc = 0, acc
      ; add = (fun (idx, acc) x -> idx + 1, add acc (f idx x))
      ; finish = (fun (_, acc) -> finish acc)
      }
  ;;

  let enumerate t = mapi t ~f:(fun idx x -> idx, x)

  let concat_map t ~f =
    let (T { acc; add; finish }) = t in
    T
      { acc
      ; add =
          (fun acc x ->
            let collection = f x in
            Collection.fold collection ~init:acc ~f:add)
      ; finish
      }
  ;;

  let concat_mapi t ~f = concat_map t ~f:(fun (idx, x) -> f idx x) |> enumerate

  let filter_map t ~f =
    let (T { acc; add; finish }) = t in
    T
      { acc
      ; add =
          (fun acc x ->
            match f x with
            | None -> acc
            | Some x' -> add acc x')
      ; finish
      }
  ;;

  let filter t ~f = filter_map t ~f:(fun x -> Option.some_if (f x) x)
  let filter_mapi t ~f = filter_map t ~f:(fun (idx, x) -> f idx x) |> enumerate
  let filteri t ~f = filter_mapi t ~f:(fun idx x -> Option.some_if (f idx x) x)

  let folding_map t ~init ~f =
    let (T { acc; add; finish }) = t in
    T
      { acc = acc, init
      ; add =
          (fun (acc, outer_acc) x ->
            let outer_acc', x' = f outer_acc x in
            add acc x', outer_acc')
      ; finish = (fun (acc, _) -> finish acc)
      }
  ;;

  let folding_filter_map t ~init ~f =
    let (T { acc; add; finish }) = t in
    T
      { acc = acc, init
      ; add =
          (fun (acc, outer_acc) x ->
            let outer_acc', x_opt = f outer_acc x in
            match x_opt with
            | None -> acc, outer_acc'
            | Some x' -> add acc x', outer_acc')
      ; finish = (fun (acc, _) -> finish acc)
      }
  ;;

  let filter_opt t = filter_map t ~f:Fn.id

  let take_while t ~f =
    folding_filter_map t ~init:`taking ~f:(fun state x ->
      match state with
      | `dropping -> `dropping, None
      | `taking ->
        (match f x with
         | true -> `taking, Some x
         | false -> `dropping, None))
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
    folding_filter_map t ~init:(`taking n) ~f:(fun state x ->
      match state with
      | `taking k ->
        (match k > 0 with
         | true -> `taking (k - 1), Some x
         | false -> `dropping, None)
      | `dropping -> `dropping, None)
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
end

let ( >>| ) = Output.( >>| )
let ( @-> ) = Input.( @-> )

let add (type elem result) (t : (elem, result) t) (elem : elem) : (elem, result) t =
  let (T { acc; add; finish }) = t in
  T { acc = add acc elem; add; finish }
;;

let add_all (type elem result) (t : (elem, result) t) (elems : elem Collection.t)
  : (elem, result) t
  =
  let (T { acc; add; finish }) = t in
  T { acc = Collection.fold elems ~init:acc ~f:add; add; finish }
;;

let finish (type result) (t : (_, result) t) =
  let (T { acc; add = _; finish }) = t in
  finish acc
;;

let collect t elems = add_all t elems |> finish
let create ~acc ~add ~finish = T { acc; add; finish }
let stateful ~add ~finish = T { acc = (); add = (fun () elem -> add elem); finish }
let fold ~init ~f = create ~acc:init ~add:f ~finish:(fun x -> x)
let call f = create ~acc:() ~add:(fun () x -> f x) ~finish:(fun () -> ())
let list_prepend l = fold ~init:l ~f:(fun l elem -> elem :: l)
let list = T { acc = []; add = (fun l elem -> elem :: l); finish = List.rev }
let list_append prefix = list >>| fun l2 -> prefix @ l2
let set cmp = fold ~init:(Set.empty cmp) ~f:Set.add

let vec' cont =
  let vec = Vec.create () in
  stateful ~add:(Vec.push_back vec) ~finish:(fun () -> cont vec)
;;

let vec () = vec' (fun x -> x)
let array () = vec' Vec.to_array
let iarray () = vec' Vec.to_iarray
let sequence () = vec' Vec.to_sequence_mutable

let queue () =
  let queue = Queue.create () in
  stateful ~add:(Queue.enqueue queue) ~finish:(fun () -> queue)
;;

let stack () =
  let stack = Stack.create () in
  stateful ~add:(Stack.push stack) ~finish:(fun () -> stack)
;;

let hash_set key =
  let set = Hash_set.create key in
  stateful ~add:(Hash_set.add set) ~finish:(fun () -> set)
;;

let add_to_vec v = call (Vec.push_back v)
let add_to_queue q = call (Queue.enqueue q)
let add_to_stack s = call (Stack.push s)
let add_to_hash_set s = call (Hash_set.add s)

module Duplicate_behavior = struct
  type ('k, 'v, 'result) t =
    | Reduce : ('v -> 'v -> 'v) -> ('k, 'v, 'v) t
    | Fold : ('k -> 'v -> 'acc) * ('acc -> 'v -> 'acc) -> ('k, 'v, 'acc) t
    | List : ('k, 'v, 'v list) t
    | Nonempty_list : ('k, 'v, 'v Nonempty_list.t) t
    | Keep_first : ('k, 'v, 'v) t
    | Keep_last : ('k, 'v, 'v) t

  let get (type k v result) (reduction : (k, v, result) t)
    : (k -> v -> result) * (result -> v -> result)
    =
    (* A little inefficient as it requires an extra map that often is just
       identity function *)
    match reduction with
    | Reduce f -> (fun _ v -> v), f
    | Fold (init, f) -> init, f
    | Keep_first -> (fun _ v -> v), fun first _ -> first
    | Keep_last -> (fun _ v -> v), fun _ last -> last
    | List -> (fun _ v -> [ v ]), fun l x -> List.cons x l
    | Nonempty_list -> (fun _ v -> Nonempty_list.[ v ]), fun l x -> Nonempty_list.cons x l
  ;;
end

let map cmp ~duplicates =
  let first, reduce = Duplicate_behavior.get duplicates in
  fold ~init:(Map.empty cmp) ~f:(fun m (k, v) ->
    Map.update m k ~f:(function
      | None -> first k v
      | Some x -> reduce x v))
;;

let hashtbl key ~duplicates () =
  let first, reduce = Duplicate_behavior.get duplicates in
  let table = Hashtbl.create key in
  stateful
    ~add:(fun (k, v) ->
      Hashtbl.update table k ~f:(function
        | None -> first k v
        | Some x -> reduce x v))
    ~finish:(fun () -> table)
;;

(* Aggregations *)

let count =
  (* must use to avoid weak *)
  T { acc = 0; add = (fun acc _ -> acc + 1); finish = (fun x -> x) }
;;

let count_if ~f = fold ~init:0 ~f:(fun acc x -> if f x then acc + 1 else acc)

let sum (type sum) (module S : Container.Summable with type t = sum) ~f =
  fold ~init:S.zero ~f:(fun sum x -> S.( + ) sum (f x))
;;

let best ~is_better =
  fold ~init:None ~f:(fun best candidate ->
    match best with
    | None -> Some candidate
    | Some prev_best ->
      Some
        (match is_better candidate ~than:prev_best with
         | true -> candidate
         | false -> prev_best))
;;

let best' ~initial ~is_better =
  fold ~init:initial ~f:(fun prev_best candidate ->
    match is_better candidate ~than:prev_best with
    | true -> candidate
    | false -> prev_best)
;;

let min ~compare = best ~is_better:(fun x ~than:prev -> compare x prev < 0)

let min' ~initial ~compare =
  best' ~initial ~is_better:(fun x ~than:prev -> compare x prev < 0)
;;

let max ~compare = best ~is_better:(fun x ~than:prev -> compare x prev > 0)

let max' ~initial ~compare =
  best' ~initial ~is_better:(fun x ~than:prev -> compare x prev > 0)
;;

module Partition = struct
  type ('a, 'b) collector = ('a, 'b) t

  module Shape = struct
    type part = T

    type _ t =
      | [] : unit t
      | ( :: ) : part * 'rest t -> ('a -> 'rest) t
  end

  module Choice = struct
    type _ t =
      | This : 'elem -> ('elem -> _) t
      | Skip : 'remaining t -> (_ -> 'remaining) t
  end

  module Choices = struct
    type ('elems, 'choice) t' =
      | [] : (unit, 'choice) t'
      | ( :: ) :
          ('elem -> 'choice) * ('elems, 'choice) t'
          -> ('elem -> 'elems, 'choice) t'

    type 'elems t = ('elems, 'elems Choice.t) t'

    let[@tail_mod_cons] rec shape'
      : type elems choice. (elems, choice) t' -> elems Shape.t
      = function
      | [] -> []
      | _ :: rest -> T :: shape' rest
    ;;

    let shape (t : _ t) = shape' t

    let of_shape (type elems) (shape : elems Shape.t) : elems t =
      let[@tail_mod_cons] rec aux
        : type sub_elems.
          sub_elems Shape.t
          -> map_choice:(sub_elems Choice.t -> elems Choice.t)
          -> (sub_elems, elems Choice.t) t'
        =
        fun shape ~map_choice ->
        match shape with
        | [] -> []
        | _ :: rest ->
          let choice x = map_choice (This x) in
          choice :: aux rest ~map_choice:(fun choice -> map_choice (Skip choice))
      in
      aux shape ~map_choice:(fun x -> x)
    ;;
  end

  module Collectors = struct
    type (_, _) t =
      | [] : (unit, unit) t
      | ( :: ) :
          ('elem, 'result) collector * ('elems, 'results) t
          -> ('elem -> 'elems, 'result -> 'results) t

    let[@tail_mod_cons] rec elems_shape
      : type elems results. (elems, results) t -> elems Shape.t
      = function
      | [] -> []
      | _ :: rest -> T :: elems_shape rest
    ;;

    let[@tail_mod_cons] rec results_shape
      : type elems results. (elems, results) t -> results Shape.t
      = function
      | [] -> []
      | _ :: rest -> T :: results_shape rest
    ;;

    let choices t = Choices.of_shape (elems_shape t)
  end

  module Results = struct
    type _ t =
      | [] : unit t
      | ( :: ) : 'result * 'results t -> ('result -> 'results) t

    let[@tail_mod_cons] rec shape : type elems. elems t -> elems Shape.t = function
      | [] -> []
      | _ :: rest -> T :: shape rest
    ;;
  end

  let[@tail_mod_cons] rec apply_choice
    : type elems results.
      (elems, results) Collectors.t -> elems Choice.t -> (elems, results) Collectors.t
    =
    fun collectors choice ->
    match choice, collectors with
    | This x, collector :: rest -> add collector x :: rest
    | Skip choice', collector :: rest -> collector :: apply_choice rest choice'
  ;;

  let[@tail_mod_cons] rec finish_all
    : type elems results. (elems, results) Collectors.t -> results Results.t
    = function
    | [] -> []
    | collector :: rest -> finish collector :: finish_all rest
  ;;

  let create' collectors = T { acc = collectors; add = apply_choice; finish = finish_all }

  let create collectors ~f =
    let choices = Collectors.choices collectors in
    f choices @-> create' collectors
  ;;

  let result ok error =
    create [ ok; error ] ~f:(fun [ choose_ok; choose_error ] -> function
      | Ok x -> choose_ok x
      | Error x -> choose_error x)
    >>| fun [ oks; errors ] -> oks, errors
  ;;

  let either first second =
    create [ first; second ] ~f:(fun [ choose1; choose2 ] -> function
      | First x -> choose1 x
      | Second x -> choose2 x)
    >>| fun [ firsts; seconds ] -> firsts, seconds
  ;;

  let collect collection ~collectors ~f = collect (create collectors ~f) collection
end

let%expect_test "Demo" =
  let input = [ Ok 1; Error 2; Ok 3; Ok 5; Error 4; Error 6 ] in
  let oks, errors =
    input |> Collection.list |> collect (Partition.result list (iarray ()))
  in
  print_s [%message (oks : int list) (errors : int iarray)];
  [%expect {| ((oks (1 3 5)) (errors (2 4 6))) |}]
;;
