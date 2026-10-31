@@ portable

open! Base

(** A lock-free multi-producer, multi-consumer queue. *)
type (!'a : value_or_null) t : value mod contended portable

(** [create ()] creates a new empty lock-free multi-producer multi-consumer queue.

    [create ~padded:true ()] creates a new empty queue that is padded so that all atomic
    references are alone on a cache line. This uses more memory, but can significantly
    improve performance by avoiding false sharing for contended queues. *)
val create : ('a : value_or_null). ?padded:bool @ local -> unit -> 'a t

(** [push t v] pushes [v] to the tail of the queue [t]. *)
val push
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended once portable unique -> unit

(** [push_head t v] pushes [v] to the head of the queue [t]. *)
val push_head
  : ('a : value_or_null).
  'a t @ local -> 'a @ contended once portable unique -> unit

(** [pop t] attempts to remove the value at the head of the queue [t], and returns
    [This v] if the queue was not empty, or [Null] otherwise. *)
val pop : 'a t @ local -> 'a or_null @ contended once portable unique

(** [pop_opt t] attempts to remove the value at the head of the queue [t], and returns
    [Some v] if the queue was not empty, or [None] otherwise. *)
val pop_opt
  : ('a : value_or_null).
  'a t @ local -> 'a option @ contended once portable unique

exception Empty

(** [pop_exn t] attempts to remove the value at the head of the queue [t], and returns [v]
    if the queue was not empty.

    @raise Empty in case the queue was empty. *)
val pop_exn : ('a : value_or_null). 'a t @ local -> 'a @ contended once portable unique

(** [peek t] is the next value that will be [pop]ped from the queue, or [Null] if the
    queue is empty.

    Note that it is possible that [pop] returns a different value after [peek] is called
    if another thread modifies the queue in the interim. *)
val peek : ('a : value mod aliased many). 'a t -> 'a or_null @ contended portable

(** [peek_opt] is like [peek], but it returns [option] instead of [or_null], so can be
    used with queues containing [value_or_null] elements. *)
val peek_opt : ('a : value mod aliased many). 'a t -> 'a option @ contended portable

(** [peek_exn] is like [peek], but it raises [Empty] if the queue is empty. *)
val peek_exn : ('a : value mod aliased many). 'a t -> 'a @ contended portable

(** [length t] is the number of values in the queue [t]. *)
val length : 'a t @ local -> int

(** [is_empty t] is [length t = 0]. *)
val is_empty : 'a t @ local -> bool
