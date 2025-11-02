open! Core
open! Async_kernel

module Scheduler : sig
  (** Schedule work from Async to be run in parallel, and await the results of that work
      from Async.

      To use this, module, first create a {!Portable_async.Parallel.Scheduler.t} by
      creating one of the async-capable parallel schedulers:

      - {!Parallel_scheduler_work_stealing}

      and passing that to {!create}. This is an expensive, resource-consuming operation,
      so should ideally be done once (or only a few times) per process. Then, you can
      dispatch work to be run in parallel by calling {!schedule}.

      {1 Example}

      {[
        open! Core
        open Async

        let main () =
          let scheduler =
            Portable_async.Parallel.Scheduler.create
              (module Parallel_scheduler_work_stealing)
              (Parallel_scheduler_work_stealing.create ())
          in
          let%bind msg =
            Portable_async.Parallel.Scheduler.parallel scheduler ~f:(fun par ->
              (* This function is run in parallel, away from the async scheduler. Any
                 [portable] computation can be done here, either directly or using the API
                 from [Parallel] to perform nested, fork-join parallelism *)
              let #(x, y) =
                Parallel.fork_join2
                  par
                  (fun (_ : Parallel.t) -> "hello from ")
                  (fun (_ : Parallel.t) -> "parallel")
              in
              x ^ y)
          in
          print_endline msg;
          return ()
        ;;
      ]} *)

  (** Type representing an (async-capable) parallel scheduler to which tasks can be
      dispatched from [Async] *)
  type t

  (** [create ?monitor (module Scheduler) scheduler] creates a new async-capable parallel
      scheduler wrapping the Parallel scheduler [scheduler].

      If tasks run on this scheduler using [schedule] raise uncaught exceptions, a
      {!Parallel.Panic.Incident.t} will be reported to the async monitor [monitor], which
      defaults to the current monitor. *)
  val create
    : 'sched.
    ?monitor:Async_kernel.Monitor.t
    -> (module Parallel_kernel.Scheduler.S_concurrent with type t = 'sched)
    -> 'sched
    -> t

  (** [parallel t ~f] schedules [f parallel] to run on the scheduler [t] and returns a
      [Deferred.t] that will become determined with the result of [f]. [parallel] is a new
      implementation of parallelism that allows [f] to perform nested parallelism *)
  val parallel
    : ('a : value mod contended).
    t -> f:(Parallel_kernel.t @ local -> 'a @ portable) @ once portable -> 'a Deferred.t

  (** [concurrent t ~f] schedules [f concurrent] to run on the scheduler [t] and returns a
      [Deferred.t] that will become determined with the result of [f]. [concurrent] is a
      new implementation of concurrency that allows [f] to perform concurrency and
      parallelism.

      If you do not need to run concurrent tasks from [f], prefer to use [parallel].
      Notably, the parallelism scheduler is not {i fair} - if you run concurrent tasks
      using this function, they can easily starve each other out. *)
  val concurrent
    : ('a : value mod contended).
    t
    -> f:(Parallel_kernel.t Concurrent.t @ local -> 'a @ portable) @ once portable
    -> 'a Deferred.t
end
