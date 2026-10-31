open! Core

module Make (Data : sig
    module Make (Key : T1) : T1
  end) =
struct
  module Key = struct
    type 'a key = ..

    type 'a t =
      { idx : int
      ; key : 'a key
      ; type_equal_exn : 'b. 'b t -> ('a, 'b) Type_equal.t
      }

    let sexp_of_t _ t = [%sexp_of: int] t.idx

    let compare (type a b) (a : a t) (b : b t) : (a, b) Typed_comparable.Ordering.t =
      match [%compare: int] a.idx b.idx |> Ordering.of_int with
      | Less -> Less
      | Greater -> Greater
      | Equal -> Equal (a.type_equal_exn b)
    ;;
  end

  module Data = struct
    include Data.Make (Key)

    type univ = Univ : 'a t * 'a Key.t -> univ
  end

  type t = Data.univ Vec.t
  type typed_key_map = t

  let create () = Vec.create ()

  let find (type a) (t : t) (key : a Key.t) : a Data.t =
    let (Univ (data, data_key)) = Vec.get t key.idx in
    let T = key.type_equal_exn data_key in
    data
  ;;

  let to_alist (type b) (t : t) (f : 'a. 'a Key.t -> 'a Data.t -> b) : b list =
    Vec.to_list t |> List.map ~f:(fun (Univ (data, key)) -> f key data)
  ;;

  module Builder = struct
    type 'a t = typed_key_map -> 'a

    let return x _s = x

    let bind t ~f s =
      let x = t s in
      f x s
    ;;

    let map = `Define_using_bind

    include functor Monad.Make

    let add (type a) (data : a Data.t) : a Key.t t =
      fun state ->
      let idx = Vec.length state in
      let open struct
        type 'a Key.key += Key : a Key.key
      end in
      let key =
        { Key.idx
        ; key = Key
        ; type_equal_exn =
            (fun (type b) (b : b Key.t) ->
              match b.key with
              | Key -> (T : (a, b) Type_equal.t)
              | _ ->
                raise_s
                  [%message
                    "type_equal_exn called on non equal types" (idx : int) (b.idx : int)])
        }
      in
      Vec.push_back state (Univ (data, key));
      key
    ;;

    let update (type a) (key : a Key.t) ~(f : a Data.t -> a Data.t) : unit t =
      fun state -> Vec.set state key.idx (Univ (f (find state key), key))
    ;;
  end

  let build (type a) (builder : a Builder.t) =
    let state = create () in
    let x = builder state in
    x, state
  ;;
end
