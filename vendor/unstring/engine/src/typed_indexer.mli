open! Core

(** This module provides a way to incrementally build a typed storage, where each item
    ['a Data.t] gets assigned a corresponding key ['a Key.t] which it can be accessed by.
    It is expressed as a functor that takes the [Data] module and mints the [_ Key.t]
    type, as well as the storage type [t].

    In order to let the ['a Data.t] reference the key type (e.g. nodes linking to other
    nodes), even though it doesn't exist yet, the [Data] module itself is a functor that
    takes the [Key] module. A good way to tie this knot can look as follows:

    {[
      module Types (Key : T1) = struct
        module Example_intermediate_type = struct
          type 'a t =
            { next : 'a Key.t
            ; value : Value.t
            }
        end

        module Data = struct
          type 'a t = 'a Example_intermediate_type.t list
        end
      end

      module State = Typed_indexer.Make (struct
          module Make (Key : T1) = struct
            module Types = Types (Key)
            include Types.Data
          end
        end)

      include Types (State.Key)
      module Key = State.Key
    ]} *)

module Make (Data : sig
    module Make (Key : T1) : T1
  end) : sig
  module Key : sig
    type 'a t : value mod non_float [@@deriving sexp_of]

    include Typed_comparable.S with type 'a t := 'a t
  end

  module Data : sig
    type 'a t = 'a Data.Make(Key).t
  end

  (** The storage type. It needs to be built using the [Builder] monad and running
      [build], as it is immutable afterwards, *)
  type t

  val find : t -> 'a Key.t -> 'a Data.t
  val to_alist : t -> ('a. 'a Key.t -> 'a Data.t -> 'b) -> 'b list

  module Builder : sig
    type 'a t

    include Monad.S with type 'a t := 'a t

    (** [add data] allocates a fresh key for [data]. *)
    val add : 'a Data.t -> 'a Key.t t

    (** [update key ~f] modifies the data at [key]. *)
    val update : 'a Key.t -> f:('a Data.t -> 'a Data.t) -> unit t
  end

  val build : 'a Builder.t -> 'a * t
end
