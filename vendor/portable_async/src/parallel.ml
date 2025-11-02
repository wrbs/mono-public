open! Core

let send_exn = Capsule.Initial.Data.wrap Async_kernel.Monitor.send_exn

module Scheduler = struct
  type t =
    | T :
        { scheduler : 'sched
        ; scheduler_module :
            (module Parallel_kernel.Scheduler.S_concurrent with type t = 'sched)
        ; scope : unit Concurrent.Scope.t
        }
        -> t

  let create ?(monitor = Async_kernel.Monitor.current ()) scheduler_module scheduler =
    let monitor = { aliased = monitor } |> Capsule.Initial.Data.wrap in
    let execution_context =
      Async_kernel.Async_kernel_scheduler.current_execution_context ()
      |> Capsule.Initial.Data.wrap
    in
    let scope =
      Await.Scope.Global.create () ~on_exit:(fun _scope maybe_exn ->
        Or_null.iter maybe_exn ~f:(fun (exn, bt) ->
          Async_kernel.Async_kernel_scheduler.portable_enqueue_job
            execution_context
            (Capsule.Data.create (fun () : _ ->
               fun #(access, { aliased = monitor }) ->
               let send_exn = Capsule.Data.unwrap ~access send_exn in
               send_exn monitor exn ~backtrace:(`This bt)))
            monitor))
    in
    T { scheduler; scheduler_module; scope }
  ;;

  let spawn (T { scheduler; scheduler_module = (module Scheduler); scope }) ~f =
    let portable_ivar = Ivar.create () in
    let scheduler = Scheduler.Expert.scheduler scheduler in
    Concurrent.Scheduler.spawn scheduler scope ~f:(fun scope parallel concurrent ->
      let result = f scope parallel concurrent in
      Ivar.fill_if_empty portable_ivar { portended = result });
    Ivar.read portable_ivar
    |> Async_kernel.Deferred.map ~f:(fun { portended } -> Modes.Contended.cross portended)
  ;;

  let parallel t ~f = spawn t ~f:(fun _scope parallel _concurrent -> f parallel)
  let concurrent t ~f = spawn t ~f:(fun _scope _parallel concurrent -> f concurrent)
end
