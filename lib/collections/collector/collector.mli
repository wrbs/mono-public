open! Core

(** An [('elem, 'result) t] is a collector that collects ['elem]s producing a ['result] *)
type (-'elem, +'result) t

(** Driving *)

val add : ('elem, 'result) t -> 'elem -> ('elem, 'result) t
val add_all : ('elem, 'result) t -> 'elem Collection.t -> ('elem, 'result) t
val finish : (_, 'result) t -> 'result
val collect : ('a, 'result) t -> 'a Collection.t -> 'result

(** Creating *)

val create
  :  acc:'acc
  -> add:('acc -> 'elem -> 'acc)
  -> finish:('acc -> 'result)
  -> ('elem, 'result) t

val fold : init:'acc -> f:('acc -> 'elem -> 'acc) -> ('elem, 'acc) t
val stateful : add:('elem -> unit) -> finish:(unit -> 'result) -> ('elem, 'result) t
val call : ('acc -> unit) -> ('acc, unit) t

(** Collection into types *)

val list : ('a, 'a list) t
val list_prepend : 'a list -> ('a, 'a list) t
val list_append : 'a list -> ('a, 'a list) t
val set : ('a, 'cmp) Comparator.Module.t -> ('a, ('a, 'cmp) Set.t) t
val sequence : unit -> ('a, 'a Sequence.t) t
val vec : unit -> ('a, 'a Vec.t) t
val array : unit -> ('a, 'a array) t
val iarray : unit -> ('a, 'a iarray) t
val queue : unit -> ('a, 'a Queue.t) t
val stack : unit -> ('a, 'a Stack.t) t
val hash_set : 'a Base.Hashtbl.Key.t -> ('a, 'a Hash_set.t) t
val add_to_vec : 'a Vec.t -> ('a, unit) t
val add_to_queue : 'a Queue.t -> ('a, unit) t
val add_to_stack : 'a Stack.t -> ('a, unit) t
val add_to_hash_set : 'a Hash_set.t -> ('a, unit) t

module Duplicate_behavior : sig
  type ('k, 'v, 'result) t =
    | Reduce : ('v -> 'v -> 'v) -> ('k, 'v, 'v) t
    | Fold : ('k -> 'v -> 'acc) * ('acc -> 'v -> 'acc) -> ('k, 'v, 'acc) t
    | List : ('k, 'v, 'v list) t
    | Nonempty_list : ('k, 'v, 'v Nonempty_list.t) t
    | Keep_first : ('k, 'v, 'v) t
    | Keep_last : ('k, 'v, 'v) t
end

val map
  :  ('k, 'cmp) Comparator.Module.t
  -> duplicates:('k, 'v, 'result) Duplicate_behavior.t
  -> ('k * 'v, ('k, 'result, 'cmp) Map.t) t

val hashtbl
  :  'k Base.Hashtbl.Key.t
  -> duplicates:('k, 'v, 'result) Duplicate_behavior.t
  -> unit
  -> ('k * 'v, ('k, 'result) Hashtbl.t) t

(** Aggregations -- see also [fold] above *)

val count : (_, int) t
val count_if : f:('a -> bool) -> ('a, int) t
val sum : (module Container.Summable with type t = 'sum) -> f:('a -> 'sum) -> ('a, 'sum) t
val min : compare:[%compare: 'a] -> ('a, 'a option) t
val max : compare:[%compare: 'a] -> ('a, 'a option) t
val min' : initial:'a -> compare:[%compare: 'a] -> ('a, 'a) t
val max' : initial:'a -> compare:[%compare: 'a] -> ('a, 'a) t
val best : is_better:('a -> than:'a -> bool) -> ('a, 'a option) t
val best' : initial:'a -> is_better:('a -> than:'a -> bool) -> ('a, 'a) t

(** Transformations *)

val ( >>| ) : ('elem, 'a) t -> ('a -> 'b) -> ('elem, 'b) t
val ( @-> ) : ('b -> 'a) -> ('a, 'result) t -> ('b, 'result) t

module Output : sig
  (** Transforming the output works like an applicative *)

  include Applicative.S2 with type ('result, 'elem) t := ('elem, 'result) t

  module Let_syntax : sig
    val return : 'result -> (_, 'result) t

    include
      Applicative.Applicative_infix2 with type ('result, 'elem) t := ('elem, 'result) t

    module Let_syntax : sig
      val return : 'result -> (_, 'result) t
      val map : ('elem, 'a) t -> f:('a -> 'b) -> ('elem, 'b) t
      val both : ('elem, 'a) t -> ('elem, 'b) t -> ('elem, 'a * 'b) t
    end
  end
end

module Input : sig
  (** Transforming the input works as a contra-map *)

  val map : ('a, 'result) t -> f:('b -> 'a) -> ('b, 'result) t
  val mapi : ('a, 'result) t -> f:(int -> 'b -> 'a) -> ('b, 'result) t
  val concat_map : ('a, 'result) t -> f:('b -> 'a Collection.t) -> ('b, 'result) t
  val concat_mapi : ('a, 'result) t -> f:(int -> 'b -> 'a Collection.t) -> ('b, 'result) t
  val filter : ('a, 'result) t -> f:('a -> bool) -> ('a, 'result) t
  val filteri : ('a, 'result) t -> f:(int -> 'a -> bool) -> ('a, 'result) t
  val filter_map : ('a, 'result) t -> f:('b -> 'a option) -> ('b, 'result) t
  val filter_mapi : ('a, 'result) t -> f:(int -> 'b -> 'a option) -> ('b, 'result) t

  val folding_map
    :  ('a, 'result) t
    -> init:'acc
    -> f:('acc -> 'b -> 'acc * 'a)
    -> ('b, 'result) t

  val folding_filter_map
    :  ('a, 'result) t
    -> init:'acc
    -> f:('acc -> 'b -> 'acc * 'a option)
    -> ('b, 'result) t

  val filter_opt : ('a, 'result) t -> ('a option, 'result) t
  val take_while : ('a, 'result) t -> f:('a -> bool) -> ('a, 'result) t
  val drop_while : ('a, 'result) t -> f:('a -> bool) -> ('a, 'result) t
  val take : ('a, 'result) t -> n:int -> ('a, 'result) t
  val drop : ('a, 'result) t -> n:int -> ('a, 'result) t
end

module Partition : sig
  val result
    :  ('ok, 'oks) t
    -> ('error, 'errors) t
    -> (('ok, 'error) Result.t, 'oks * 'errors) t

  val either
    :  ('first, 'firsts) t
    -> ('second, 'seconds) t
    -> (('first, 'second) Either.t, 'firsts * 'seconds) t

  (** {2 Custom partitions}

      You can make custom partitionings of any length/collected types.

      Depends on some list syntax GADTs, but not too bad if you follow examples.

      {[
        let example
          :  'a option Or_error.t list
          -> somes:'a iarray * none_count:int * errors:Error.t list
          =
          fun l ->
          let [ somes; none_count; errors ] =
            Collector.Partition.collect
              (Collection.list l)
              ~collectors:Collector.[ iarray (); count; list ]
              ~f:(fun [ some; none; error ] -> function
                | Ok (Some x) -> some x
                | Ok None -> none ()
                | Error x -> error x)
          in
          ~somes, ~none_count, ~errors
        ;;

        (* or to implement partition_result above *)
        let partition_result collect_ok collect_error =
          let%map.Collector.Output [ oks; errors ] =
            Collector.Partition.create
              [ collect_ok; collect_error ]
              ~f:(fun [ ok; error ] -> function
              | Ok x -> ok x
              | Error x -> error x)
          in
          oks, errors
        ;;
      ]} *)

  include Partition_types_intf.S with type ('a, 'b) collector := ('a, 'b) t

  val create
    :  ('elems, 'results) Collectors.t
    -> f:('elems Choices.t -> 'a -> 'elems Choice.t)
    -> ('a, 'results Results.t) t

  val collect
    :  'a Collection.t
    -> collectors:('elems, 'results) Collectors.t
    -> f:('elems Choices.t -> 'a -> 'elems Choice.t)
    -> 'results Results.t

  (** For specialized use cases: see [Collectors.choices]/[Choices.of_shape]/[...] for the
      things you'll need to make use of this *)
  val create' : ('elems, 'results) Collectors.t -> ('elems Choice.t, 'results Results.t) t
end
