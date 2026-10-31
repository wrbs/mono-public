module type S = sig
  type 'context t constraint 'context = ('i, 'o) Lws_context.t

  (** {2 API to interact with tasks} *)

  (** Schedule a computation to be run. If you want to schedule and poll the task
      immediately, consider using [run]. *)
  val schedule_task
    :  ?trigger:Lws.Trigger.t
         (** When [trigger] is not provided, the runtime will choose a default trigger.
             This will raise an exception if the runtime doesn't allow a default trigger
             for the particular execution. *)
    -> here:[%call_pos]
    -> 'context t
    -> (Lws.Handler.t @ local -> 'context -> 'a)
    -> 'a Lws.Task.t

  (** Given a task, poll until completion *)
  val poll_task : _ t -> 'a Lws.Task.t -> 'a

  (** similar to [run t task] is identical to [poll_task t (schedule_task t task)] *)
  val run
    :  ?trigger:Lws.Trigger.t
    -> here:[%call_pos]
    -> 'context t
    -> (Lws.Handler.t @ local -> 'context -> 'a)
    -> 'a

  val resolve_trigger_by_clock_name : _ t -> string -> Lws.Trigger.t option
  val context : 'context t -> 'context
end

module type Lws_runtime = sig
  module type S = S

  include S

  module To_polymorphic (X : S) : sig
    val to_polymorphic : 'context X.t -> 'context t
  end
end
