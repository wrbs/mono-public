open! Core

type +'a t = { iter : f:('a -> unit) @ local -> unit } [@@unboxed]

include Monad.S with type 'a t := 'a t

(** {2 Creation} *)

val empty : _ t
val singleton : 'a -> 'a t

[%%template:
[@@@alloc.default a @ m = (heap @ global, stack @ local)]

val create : (f:('a -> unit) @ local -> unit) @ m -> 'a t @ m
val stateful : (unit -> 'a t) @ m -> 'a t @ m
val repeatedly_call : f:('a -> 'a) @ m -> on:'a -> 'a t @ m
val unfold : 'state -> f:('state -> ('state * 'a) option) @ m -> 'a t @ m]

val range
  :  ?stride:int (** default = 1 *)
  -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
  -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
  -> int
  -> int
  -> int t

(** [range'] is analogous to [range] for general start/stop/stride types. [range'] raises
    if [stride x] returns [x] or if the direction that [stride x] moves [x] changes from
    one call to the next. *)
val range'
  :  compare:('a -> 'a -> int)
  -> stride:('a -> 'a)
  -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
  -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
  -> 'a
  -> 'a
  -> 'a t

(** {3 Conversion in} *)

val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t
val of_iarray : 'a iarray -> 'a t
val of_vec : 'a Vec.t -> 'a t
val of_queue : 'a Queue.t -> 'a t
val of_stack : 'a Stack.t -> 'a t
val of_sequence : 'a Sequence.t -> 'a t
val of_map : ('k, 'v, _) Map.t -> ('k * 'v) t
val of_map_keys : ('k, _, _) Map.t -> 'k t
val of_map_data : (_, 'v, _) Map.t -> 'v t
val of_set : ('a, _) Set.t -> 'a t
val of_hashtbl : ('k, 'v) Hashtbl.t -> ('k * 'v) t
val of_hashtbl_keys : ('k, _) Hashtbl.t -> 'k t
val of_hashtbl_data : (_, 'v) Hashtbl.t -> 'v t

(** {2 Consumption} *)

val iter : 'a t @ local -> f:('a -> unit) @ local -> unit
val iteri : 'a t @ local -> f:(int -> 'a -> unit) @ local -> unit
val iter_while : 'a t @ local -> f:('a -> bool) @ local -> unit
val fold : 'a t @ local -> init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc
val mem : 'a t @ local -> 'a -> equal:[%equal: 'a] @ local -> bool
val find : 'a t @ local -> f:('a -> bool) @ local -> 'a option
val find_map : 'a t @ local -> f:('a -> 'b option) @ local -> 'b option
val findi : 'a t @ local -> f:(int -> 'a -> bool) @ local -> 'a option
val find_mapi : 'a t @ local -> f:(int -> 'a -> 'b option) @ local -> 'b option
val for_all : 'a t @ local -> f:('a -> bool) @ local -> bool
val exists : 'a t @ local -> f:('a -> bool) @ local -> bool
val max_elt : 'a t @ local -> compare:[%compare: 'a] @ local -> 'a option
val min_elt : 'a t @ local -> compare:[%compare: 'a] @ local -> 'a option

val sum
  :  (module Container.Summable with type t = 'sum)
  -> 'a t @ local
  -> f:('a -> 'sum) @ local
  -> 'sum

(** {3 Conversion out} *)

val to_list : 'a t @ local -> 'a list
val to_vec : ?size_hint:int -> 'a t @ local -> 'a Vec.t
val to_queue : ?size_hint:int -> 'a t @ local -> 'a Queue.t
val to_stack : 'a t @ local -> 'a Stack.t
val to_array : ?size_hint:int -> 'a t @ local -> 'a array
val to_iarray : ?size_hint:int -> 'a t @ local -> 'a iarray
val to_set : ('a, 'cmp) Comparator.Module.t -> 'a t @ local -> ('a, 'cmp) Set.t
val to_hash_set : ?size:int -> 'a Base.Hashtbl.Key.t -> 'a t @ local -> 'a Hash_set.t
val to_sequence : ?size_hint:int -> 'a t @ local -> 'a Sequence.t

(** filling *)

val add_to_vec : 'a t @ local -> vec:'a Vec.t -> unit
val add_to_queue : 'a t @ local -> queue:'a Queue.t -> unit
val add_to_stack : 'a t @ local -> stack:'a Stack.t -> unit
val add_to_hash_set : 'a t @ local -> set:'a Hash_set.t -> unit
val fill_array : 'a t @ local -> array:'a array @ local -> unit

(* TODO: more options (multi/reduce/...) *)

val to_map
  :  ('k, 'cmp) Comparator.Module.t
  -> ('k * 'v) t
  -> [ `Duplicate_key of 'k | `Ok of ('k, 'v, 'cmp) Map.t ]

val to_map_exn : ('k, 'cmp) Comparator.Module.t -> ('k * 'v) t -> ('k, 'v, 'cmp) Map.t

val to_hashtbl_reduce
  :  'k Base.Hashtbl.Key.t
  -> ('k * 'v) t
  -> reduce:('k -> 'v -> 'v -> 'v)
  -> ('k, 'v) Hashtbl.t

val to_hashtbl_exn : 'k Base.Hashtbl.Key.t -> ('k * 'v) t -> ('k, 'v) Hashtbl.t

(** {3 Transformation} *)

[%%template:
[@@@alloc.default a @ m = (heap @ global, stack @ local)]

val forever : 'a t @ m -> 'a t @ m (* infinite loop on empty *)
val enumerated : 'a t @ m -> (int * 'a) t @ m
val cons : 'a -> 'a t @ m -> 'a t @ m
val append : 'a t @ m -> 'a t @ m -> 'a t @ m
val concat : 'a t t @ m -> 'a t @ m
val concat_list : 'a t list @ m -> 'a t @ m
val take_while : 'a t @ m -> f:('a -> bool) @ m -> 'a t @ m
val drop_while : 'a t @ m -> f:('a -> bool) @ m -> 'a t @ m
val take : 'a t @ m -> n:int -> 'a t @ m
val drop : 'a t @ m -> n:int -> 'a t @ m
val map : 'a t @ m -> f:('a -> 'b) @ m -> 'b t @ m
val mapi : 'a t @ m -> f:(int -> 'a -> 'b) @ m -> 'b t @ m
val concat_map : 'a t @ m -> f:('a -> 'b t) @ m -> 'b t @ m
val concat_mapi : 'a t @ m -> f:(int -> 'a -> 'b t) @ m -> 'b t @ m
val concat_map_list : 'a t @ m -> f:('a -> 'b list) @ m -> 'b t @ m
val concat_map_listi : 'a t @ m -> f:(int -> 'a -> 'b list) @ m -> 'b t @ m
val filter : 'a t @ m -> f:('a -> bool) @ m -> 'a t @ m
val filteri : 'a t @ m -> f:(int -> 'a -> bool) @ m -> 'a t @ m
val filter_map : 'a t @ m -> f:('a -> 'b option) @ m -> 'b t @ m
val filter_mapi : 'a t @ m -> f:(int -> 'a -> 'b option) @ m -> 'b t @ m
val folding_map : 'a t @ m -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) @ m -> 'b t @ m

val folding_filter_map
  :  'a t @ m
  -> init:'acc
  -> f:('acc -> 'a -> 'acc * 'b option) @ m
  -> 'b t @ m

val filter_opt : 'a option t @ m -> 'a t @ m
val group : 'a t @ m -> break:('a -> 'a -> bool) @ m -> 'a t t @ m

val group_map
  :  'a t @ m
  -> break:('a -> 'a -> bool) @ m
  -> f:('a t @ local -> 'b) @ m
  -> 'b t @ m

val chunks_of : 'a t @ m -> length:int -> 'a t t @ m
val chunks_of_map : 'a t @ m -> length:int -> f:('a t @ local -> 'b) @ m -> 'b t @ m]

(** {3 Caching} *)

(** Caches the iterator to an iarray (if it isn't already cached).

    While caching, a [Vec.t] is used to store intermediate values. [size_hint] defines the
    initial capacity of this vector: set it to the length if known *)
val cache : ?size_hint:int -> 'a t @ local -> 'a t

(** [length] returns instantly for cached arrays, but otherwise scans the entire iterator *)
val length : _ t @ local -> int

val cache_with_length : ?size_hint:int -> 'a t @ local -> 'a t * length:int

(** Returns a reversed iterator over the values. Needs to cache any iterators not already
    cached: [size_hint] is used for this as in [cache].

    Does no work for iterators already cached *)
val rev : ?size_hint:int -> 'a t @ local -> 'a t

val sort : ?size_hint:int -> 'a t -> compare:[%compare: 'a] -> 'a t

module Using_effects : sig
  module Unique_driver : sig
    type 'a iter := 'a t
    type 'a t : value mod contended many

    type 'a step =
      | Next of 'a @@ aliased global * 'a t
      | Done

    val create : 'a iter -> 'a t @ unique
    val step : 'a t @ unique -> 'a step @ unique
  end

  module Driver : sig
    type 'a iter := 'a t
    type 'a t : mutable_data with 'a Unique_driver.t

    val create : 'a iter -> 'a t
    val next : 'a t -> 'a option
  end

  [%%template:
  [@@@alloc.default a @ m = (heap @ global, stack @ local)]

  val zip : 'a t @ m -> 'b t -> ('a * 'b) t @ m

  val zip_full
    :  'a t @ m
    -> 'b t
    -> [ `Left of 'a | `Both of 'a * 'b | `Right of 'b ] t @ m

  val map2 : 'a t @ m -> 'b t -> f:('a -> 'b -> 'c) @ m -> 'c t @ m
  val mapi2 : 'a t @ m -> 'b t -> f:(int -> 'a -> 'b -> 'c) @ m -> 'c t @ m
  val map3 : 'a t @ m -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'd) @ m -> 'd t @ m
  val mapi3 : 'a t @ m -> 'b t -> 'c t -> f:(int -> 'a -> 'b -> 'c -> 'd) @ m -> 'd t @ m]

  val round_robin : 'a t t -> 'a t
end
