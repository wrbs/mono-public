open! Core

(* All these work where there's no shrinker *)

module type S_shrinker_and_observer = sig
  type t [@@deriving quickcheck ~shrinker ~observer]
end

module Hashable (M : sig
    type t [@@deriving quickcheck ~generator, hash]
  end) : S_shrinker_and_observer with type t := M.t

module To_hashable
    (Hash : sig
       type t [@@deriving hash]
     end)
    (M : sig
       type t [@@deriving quickcheck ~generator]

       val to_hashable : t -> Hash.t
     end) : S_shrinker_and_observer with type t := M.t

module To_observable
    (Observe : sig
       type t [@@deriving quickcheck ~observer]
     end)
    (M : sig
       type t [@@deriving quickcheck ~generator]

       val to_observable : t -> Observe.t
     end) : S_shrinker_and_observer with type t := M.t

module Sexpable (M : sig
    type t [@@deriving quickcheck ~generator, sexp_of]
  end) : S_shrinker_and_observer with type t := M.t
