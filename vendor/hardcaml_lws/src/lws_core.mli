(** The core scheduling runtime logic around LWS.

    Every computation / task in LWS has an associated trigger. The trigger

    There is two main primitives in the LWS scheduler:
    - [step] - which yields control to the scheduler to run the other task up to the
      synchronization point. The control flow will continue
    - [spawn] - this spwans a task. The spawned task will inherit the parent task' trigger
      if it's not specified.

    The [Evaluator] module provides functionalities to step through a task written with
    LWS. *)

open Core

(** [Task.t] is a handle for a spawned task. It can be used to read the result of the
    task, or to kill it. *)
module Task : sig
  module State : sig
    type 'a t =
      | Pending
      | Resolved of 'a
      | Killed
    [@@deriving sexp_of]
  end

  type 'a t [@@deriving sexp_of]

  val read : 'a t -> 'a or_null
  val state : 'a t -> 'a State.t

  (** Kill the task. The task will stop executing on the next step of the evaluator.
      Killing an already-resolved or already-killed task is a no-op. *)
  val kill : 'a t -> unit

  val is_pending : 'a t -> bool
end

(** [Trigger.t] refers to a trigger originating from a simulator. This corresponds to an
    event that triggers a task to make progress. The overwhelmingly common case where a
    trigger is created is on a clock tick. But it doesn't need to be restricted as such. *)
module Trigger : sig
  type t = Lws_trigger.t [@@deriving sexp_of]

  (** Every call to Trigger.create created a new unique object. The string argument is a
      purely descriptive name. *)
  val create : created_at:[%call_pos] -> string -> t
end

(** [Handler.t] is the type that is passed around in computation as an accessor to the
    scheduler's primitives (step and spawn) *)
module Handler : sig
  type t
end

(** Yield control of the computation to the evaluator. If this is called in a root
    computation, the evaluator will move on to other computation that has been spawned. *)
val step : created_at:Source_code_position.t -> Handler.t @ local -> unit

(** Spawn a child computation. *)
val spawn
  :  created_at:Source_code_position.t
  -> trigger:Trigger.t option
  -> Handler.t @ local
  -> (Handler.t @ local -> 'a)
  -> 'a Task.t

(** Evaluator is an object is capable of scheduling and evaluating LWS tasks. *)
module Evaluator : sig
  type t

  val create : unit -> t

  val add_task
    :  t
    -> created_at:Source_code_position.t
    -> trigger:Trigger.t
    -> (Handler.t @ local -> 'a)
    -> 'a Task.t

  (** Calling [step] will advance all the computation that matches _any_ of the provided
      triggers by one step. *)
  val step : t -> Trigger.t iarray @ local -> unit

  val step_number : t -> int
end
