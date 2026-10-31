open! Core

module Definitions = struct
  (** Creating a [t] requires both a comparator and a hash function. The particular modes
      of comparator and hash function are given by
      [Comparator_hashable.S [@modality portable]]. *)

  module Comparator_hashable = struct
    module type%template [@mode m = (local, global)] S = sig
      type t [@@deriving (compare [@mode.explicit m]), sexp_of]
      type comparator_witness

      val hashable : t Hashtbl.Hashable.t
      val comparator : (t, comparator_witness) Comparator.t
    end

    module type%template S = sig @@ portable
      type comparator_witness : value mod portable

      include S with type comparator_witness := comparator_witness
    end
    [@@modality portable]

    type%template ('a, 'cw) t =
      ((module S with type t = 'a and type comparator_witness = 'cw)[@modality p])
    [@@modality p = (portable, nonportable)]
  end
end

module type Portable_memo = sig
  include module type of struct
    include Definitions
  end

  (** NOTE: this library is not optimized at all. You'd probably be better served by a
      parallel hashtbl. We've just written this library as a mostly drop-in replacement
      for [Memo] as a stopgap.

      This library is a way of maintaining mostly-read caches. It maintains a per-domain
      hashtbl cache, and synchronizes between domains using an atomic map.

      The two main entry points to this library are:
      - [create]
      - [memo]

      Suppose [let memo = general f]. [memo x] is guaranteed to return the same result on
      all domains, and [f] will only be run once. *)

  type ('a, 'b) t : value mod contended portable

  val find : ('a, 'b) t -> 'a @ portable -> 'b option @ portable @@ portable

  (** Returns the value mapped to [key] for all domains, which may or may not be [data]
      depending on whether other domains are racing to set [key]. *)
  val add
    :  ('a, 'b) t
    -> key:'a @ portable
    -> data:'b @ portable
    -> 'b @ portable
    @@ portable

  val create
    : ('a : value mod contended) ('b : value mod contended) ('cw : value mod portable).
    (('a, 'cw) Comparator_hashable.t[@modality portable]) -> ('a, 'b) t
    @@ portable

  val to_alist : ('a, 'b) t -> ('a * 'b) list @ portable @@ portable

  (** A portable version of [Memo]. The provided [f] is called at most once per unique
      argument, even if multiple domains try to force it. *)
  val memo
    : ('a : value mod contended) ('b : value mod contended) ('cw : value mod portable).
    (('a, 'cw) Comparator_hashable.t[@modality portable])
    -> ('a @ portable -> 'b @ portable) @ portable unyielding
    -> ('a @ portable -> 'b @ portable) @ portable unyielding
    @@ portable

  (** Support using a non-atomic implementation from templatized code. *)
  module Templated : sig
    include module type of struct
      let%template memo = memo [@@mode portable]
    end

    val memo : ('a, 'cw) Comparator_hashable.t -> ('a -> 'b) -> ('a -> 'b)
  end
end
