open! Core

module Ordering = struct
  type ('a, 'b) t =
    | Less
    | Greater
    | Equal of ('a, 'b) Type_equal.t

  let untyped (type a b) : (a, b) t -> Core.Ordering.t = function
    | Less -> Less
    | Greater -> Greater
    | Equal _ -> Equal
  ;;
end

module type S = sig
  type 'a t : value mod non_float [@@deriving sexp_of]

  val compare : 'a t -> 'b t -> ('a, 'b) Ordering.t
end

module Make_map
    (Key : S)
    (Data : sig
       type 'a t [@@deriving sexp_of]
     end) =
struct
  module Packed_key = struct
    type t = T : _ Key.t -> t [@@unboxed]

    let sexp_of_t (T key) = [%sexp_of: _ Key.t] key
    let compare (T a) (T b) = Key.compare a b |> Ordering.untyped |> Core.Ordering.to_int

    include functor Comparable.Make_plain
  end

  module Packed_key_data = struct
    type t = T : ('a Key.t * 'a Data.t) -> t

    let sexp_of_t (T (_, data)) = [%sexp_of: _ Data.t] data
  end

  type t = Packed_key_data.t Packed_key.Map.t [@@deriving sexp_of]

  let empty = Packed_key.Map.empty

  let find (type a) (t : t) (key : a Key.t) : a Data.t option =
    match Map.find t (T key) with
    | None -> None
    | Some (T (key', data)) ->
      let Type_equal.T =
        match Key.compare key key' with
        | Less | Greater -> raise_s [%message "BUG in Key.compare"]
        | Equal x -> x
      in
      Some data
  ;;

  let set (type a) (t : t) ~(key : a Key.t) ~(data : a Data.t) =
    Map.set t ~key:(T key) ~data:(T (key, data))
  ;;

  let keys (t : t) (f : 'a. 'a Key.t -> 'b) =
    Map.keys t |> List.map ~f:(fun (T key) -> f key)
  ;;

  let map (t : t) ~(f : 'a. 'a Data.t -> 'a Data.t) =
    Map.map t ~f:(fun (T (key, data)) -> Packed_key_data.T (key, f data))
  ;;
end
