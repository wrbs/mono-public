open! Core

module type S_shrinker_and_observer = sig
  type t [@@deriving quickcheck ~shrinker ~observer]
end

module Hashable (M : sig
    type t [@@deriving quickcheck ~generator, hash]
  end) : S_shrinker_and_observer with type t := M.t = struct
  let quickcheck_observer = Quickcheck.Observer.of_hash (module M)
  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

module To_observable
    (Observe : sig
       type t [@@deriving quickcheck ~observer]
     end)
    (M : sig
       type t [@@deriving quickcheck ~generator]

       val to_observable : t -> Observe.t
     end) : S_shrinker_and_observer with type t := M.t = struct
  let quickcheck_observer =
    [%quickcheck.observer: Observe.t] |> Quickcheck.Observer.unmap ~f:M.to_observable
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

module To_hashable
    (Hash : sig
       type t [@@deriving hash]
     end)
    (M : sig
       type t [@@deriving quickcheck ~generator]

       val to_hashable : t -> Hash.t
     end) : S_shrinker_and_observer with type t := M.t = struct
  let quickcheck_observer =
    Quickcheck.Observer.of_hash (module Hash)
    |> Quickcheck.Observer.unmap ~f:M.to_hashable
  ;;

  let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
end

module Sexpable (M : sig
    type t [@@deriving quickcheck ~generator, sexp_of]
  end) : S_shrinker_and_observer with type t := M.t =
  To_observable
    (Sexp)
    (struct
      include M

      let to_observable = sexp_of_t
    end)
