open! Core

type 'a t =
  | Empty : 'a t
  | Return : 'a -> 'a t
  | List : 'a list -> 'a t
  | List_map : 'a list * ('a -> 'b) -> 'b t
  | Cons : 'a * 'a t Lazy.t -> 'a t
  | Concat : 'a t list -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Recurse :
      { init : 'acc
      ; f : 'acc -> [ `Finish of 'a | `Continue of 'acc ] t
      }
      -> 'a t
  | Lazy : 'a t Lazy.t -> 'a t
[@@deriving sexp_of]

let bind t ~f =
  match t with
  | Empty -> Empty
  | Return x -> f x
  | t -> Bind (t, f)
;;

let rec next : type a. a t -> (a * a t) option = function
  | Empty -> None
  | Return x -> Some (x, Empty)
  | List [] -> None
  | List (x :: xs) -> Some (x, List xs)
  | List_map ([], _) -> None
  | List_map (x :: xs, f) -> Some (f x, List_map (xs, f))
  | Cons (x, t) -> Some (x, Lazy t)
  | Concat [] -> None
  | Concat (t :: ts) ->
    (match next t with
     | None -> next (Concat ts)
     | Some (v, t) -> Some (v, Concat (t :: ts)))
  | Map (t, f) ->
    (match next t with
     | None -> None
     | Some (v, t) -> Some (f v, Map (t, f)))
  | Bind (t, f) ->
    (match next t with
     | None -> None
     | Some (v, t) -> next (Concat [ f v; Bind (t, f) ]))
  | Recurse { init; f } ->
    bind (f init) ~f:(function
      | `Finish x -> Return x
      | `Continue acc -> Recurse { init = acc; f })
    |> next
  | Lazy t -> next (force t)
;;

let empty = Empty

let of_list = function
  | [] -> Empty
  | [ x ] -> Return x
  | xs -> List xs
;;

let of_list_map xs ~f =
  match xs with
  | [] -> Empty
  | [ x ] -> Return (f x)
  | xs -> List_map (xs, f)
;;

let concat ts = Concat ts
let return x = Return x
let map t ~f = Map (t, f)
let recurse ~init ~f = Recurse { init; f }
let cons x t = Cons (x, t)

let cache t =
  let rec aux t =
    lazy
      (match next t with
       | None -> Empty
       | Some (v, t) -> Cons (v, aux t))
  in
  Lazy (aux t)
;;
