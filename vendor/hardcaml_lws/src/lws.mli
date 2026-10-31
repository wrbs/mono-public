(** Entry point for writing testbenches with LWS. Testbench implementation code should
    primarily interact with this module.

    To execute a testbench, users should use the backend modules. See {!Hardcaml_lws} for
    the available backends. *)

open Core
open Hardcaml
module Before_and_after_edge = Before_and_after_edge

(** Shared configuration that is common across all LWS backends. *)
module Config : sig
  type t =
    { timeout : int option
    ; auto_label_hierarchical_ports : bool
    ; assertions : bool
    }

  val default : t
end

module Task : sig
  module State = Lws_core.Task.State

  type 'a t = 'a Lws_core.Task.t

  val read : 'a t -> 'a or_null
  val state : 'a t -> 'a State.t

  (** Kill the task. The task will stop executing on the next step of the evaluator.
      Killing an already-resolved or already-killed task is a no-op. *)
  val kill : 'a t -> unit

  val is_pending : 'a t -> bool
end

(** A [Trigger.t] refers to an impulse that happens in an external stimulus from the
    testbench environment that causes a computation to step forward. An example of an
    impulse is a rising edge of a clock from a circuit. *)
module Trigger : sig
  type t = Lws_core.Trigger.t
end

(** [Handler.t] is the local object that the user pass around to interact with the core
    LWS runtime. *)
module Handler : sig
  type t = Lws_core.Handler.t
end

(** Steps the simulation of the local thunk. This will yield control. *)
val step : here:[%call_pos] -> ?n:int -> Handler.t @ local -> unit

(** Alias for [step]. *)
val cycle : here:[%call_pos] -> ?n:int -> Handler.t @ local -> unit

type ('a, 'ret) spawn :=
  ?trigger:Trigger.t
  -> here:[%call_pos]
  -> Handler.t @ local
  -> (Handler.t @ local -> 'a)
  -> 'ret

(** Spawns a new computation with the given trigger. Will inherit the current task's
    trigger if none is specified.

    If the given [trigger] argument matches the current trigger that fired, the
    computation will be enqueued to run immediately. It won't wait for the next time step
    of the provided trigger. *)
val spawn : ('a, 'a Task.t) spawn

(** Similar to [spawn], but disregards the Task.t object *)
val spawn_unit : (unit, unit) spawn

(** Similar to [spawn], but disregards the Task.t object *)
val spawn_never_returns : (never_returns, unit) spawn

(** Steps the current computation until the given Task.t resolves *)
val wait : here:[%call_pos] -> Handler.t @ local -> 'a Task.t -> 'a

(** Steps the current computation until the thunk returns Some . Evaluates the thunk first
    before the first invocation of [Lws.step]. *)
val step_until_some
  :  here:[%call_pos]
  -> Handler.t @ local
  -> (unit -> 'a option) @ local
  -> 'a

(** Similar to [step_until_some], but works with or_null. *)
val step_until_this
  :  here:[%call_pos]
  -> Handler.t @ local
  -> (unit -> 'a or_null) @ local
  -> 'a
