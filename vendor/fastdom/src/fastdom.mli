open! Core

(** [fastdom] provides an API that allows users to batch reads of and writes to the DOM in
    a way that minimizes layout thrashing. It does __not__ prevent users from reading in a
    write and vice-versa. Instead, it relies on the user to implement the correct behavior *)

(** [read_then_write] ensures that your reads and writes happen in the correct order but
    does __not__ prevent you from reading in a write and vice versa. This means that
    layout thrashing is still possible if you are using the method incorrectly *)
val read_then_write
  : 'read_output.
  read:(unit -> 'read_output) -> write:('read_output -> unit) -> unit

(** [write] is a write without a read. If you need to retrieve a value from the DOM for
    the write, use [read_then_write] *)
val write : f:(unit -> unit) -> unit

(** [read] can be useful if you are planning to store the value of a DOM read somewhere
    and do not plan on writing to the DOM at all *)
val read : f:(unit -> unit) -> unit

module Expert : sig
  (** [run_actions] runs all the actions in the queue.

      You do not need to call this youself if you are using [Bonsai_web].

      This should be called on every frame. Ensure that all reads of the DOM occur before
      this function call, and all writes occur after this function call. *)
  val run_actions : unit -> unit

  (** [run_reads] runs all the reads in the queue, then returns a callback that will
      execute all writes *)
  val run_reads : unit -> [ `Run_writes of unit -> unit ]
end

module For_testing : sig
  val num_elements_in_queue : unit -> int
end
