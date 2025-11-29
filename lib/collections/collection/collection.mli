open! Core

type +'a t

val empty : _ t
val singleton : 'a -> 'a t
val repeatedly_call : f:('a -> 'a) -> on:'a -> 'a t
val unfold : 'state -> f:('state -> ('state * 'a) option) -> 'a t

(** Common containers *)

val list : 'a list -> 'a t
val sequence : 'a Sequence.t -> 'a t
val set : ('a, _) Set.t -> 'a t
val vec : 'a Vec.t -> 'a t
val array : 'a array -> 'a t
val iarray : 'a iarray -> 'a t
val queue : 'a Queue.t -> 'a t
val stack : 'a Stack.t -> 'a t
val hash_set : 'a Hash_set.t -> 'a t
val map_entries : ('k, 'v, _) Map.t -> ('k * 'v) t
val map_keys : ('k, _, _) Map.t -> 'k t
val map_data : (_, 'v, _) Map.t -> 'v t
val hashtbl_entries : ('k, 'v) Hashtbl.t -> ('k * 'v) t
val hashtbl_keys : ('k, _) Hashtbl.t -> 'k t
val hashtbl_data : (_, 'v) Hashtbl.t -> 'v t

(** Custom creators *)

val create
  :  fold:('acc. init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc)
  -> fold_until:
       ('acc 'final.
        init:'acc
        -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t) @ local
        -> finish:('acc -> 'final) @ local
        -> 'final)
  -> 'a t

val create_of_fold_until
  :  ('acc 'final.
      init:'acc
      -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t) @ local
      -> finish:('acc -> 'final) @ local
      -> 'final)
  -> fold:[ `pointless_because_infinite | `derive_it_for_me ]
  -> 'a t

val is_definitely_infinite : _ t -> bool

val create_of_fold_with_exceptions__slow_on_web
  :  ('acc. init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc)
  -> 'a t

(** Ranges *)

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
  :  compare:[%compare: 'a]
  -> stride:('a -> 'a)
  -> ?start:[ `inclusive | `exclusive ] (** default = `inclusive *)
  -> ?stop:[ `inclusive | `exclusive ] (** default = `exclusive *)
  -> 'a
  -> 'a
  -> 'a t

(** Combinators *)

val forever : 'a t -> 'a t (* infinite loop on empty *)
val enumerated : 'a t -> (int * 'a) t
val cons : 'a -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t
val concat : 'a t t -> 'a t
val take_while : 'a t -> f:('a -> bool) -> 'a t
val drop_while : 'a t -> f:('a -> bool) -> 'a t
val take : 'a t -> n:int -> 'a t
val drop : 'a t -> n:int -> 'a t
val map : 'a t -> f:('a -> 'b) -> 'b t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val concat_mapi : 'a t -> f:(int -> 'a -> 'b t) -> 'b t
val filter : 'a t -> f:('a -> bool) -> 'a t
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t
val folding_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'b t
val folding_filter_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b option) -> 'b t
val folding_take_while : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc option) -> 'a t
val filter_opt : 'a option t -> 'a t

(** Container functions *)

include sig
  include Container.S1 with type 'a t := 'a t

  val length : _ t -> int
  val is_empty : _ t -> bool
  val iter : 'a t -> f:('a -> unit) -> unit

  val iter_until
    :  'a t
    -> f:('a -> (unit, 'final) Continue_or_stop.t) @ local
    -> finish:(unit -> 'final) @ local
    -> 'final

  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) @ local -> 'acc

  val fold_until
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t) @ local
    -> finish:('acc -> 'final) @ local
    -> 'final

  val fold_result
    :  'a t
    -> init:'acc
    -> f:('acc -> 'a -> ('acc, 'e) Result.t) @ local
    -> ('acc, 'e) Result.t

  val mem : 'a t -> 'a -> equal:[%equal: 'a] @ local -> bool
  val exists : 'a t -> f:('a -> bool) -> bool
  val for_all : 'a t -> f:('a -> bool) -> bool
  val count : 'a t -> f:('a -> bool) -> int

  val sum
    :  (module Container.Summable with type t = 'sum)
    -> 'a t
    -> f:('a -> 'sum)
    -> 'sum

  val find : 'a t -> f:('a -> bool) -> 'a option
  val find_map : 'a t -> f:('a -> 'b option) -> 'b option
  val min_elt : 'a t -> compare:[%compare: 'a] @ local -> 'a option
  val max_elt : 'a t -> compare:[%compare: 'a] @ local -> 'a option
  val to_list : 'a t -> 'a list
  val to_array : 'a t -> 'a array
end
