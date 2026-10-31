open Core

module Task = struct
  module State = struct
    type 'a t =
      | Pending
      | Resolved of 'a
      | Killed
    [@@deriving sexp_of]
  end

  type 'a t = 'a State.t ref [@@deriving sexp_of]

  let create () = ref State.Pending

  let read (t : _ t) =
    match !t with
    | Resolved a -> This a
    | Pending | Killed -> Null
  ;;

  let state t = !t
  let resolve t a = t := State.Resolved a

  let kill (t : _ t) =
    match !t with
    | Pending -> t := Killed
    | Resolved _ | Killed -> ()
  ;;

  let is_pending (t : _ t) =
    match !t with
    | Pending -> true
    | Resolved _ | Killed -> false
  ;;
end

module Trigger = Lws_trigger

module Effect_ops = struct
  type ('a, 'effect) spawn =
    { created_at : Source_code_position.t
    ; computation : 'effect Handled_effect.Handler.t @ local -> 'a
    ; trigger : Trigger.t option
    }

  type ('a, 'effect) t =
    | Step : Source_code_position.t -> (unit, 'effect) t
    | Spawn : ('a, 'effect) spawn -> ('a Task.t aliased_many, 'effect) t
end

module E = Handled_effect.Make_rec (Effect_ops)
module Handler = E.Handler

module Exn_and_backtrace = struct
  type t =
    { exn : Exn.t
    ; backtrace : Backtrace.t option
    }
end

module Evaluator = struct
  module Thunk_evaluator = struct
    module State = struct
      type 'a t =
        | Unstarted of (Handler.t @ local -> 'a)
        | Running of (unit, 'a, unit) E.Continuation.t Unique.Once.t
        | Finished of 'a
        | Killed

      let is_finished = function
        | Finished _ -> true
        | Unstarted _ | Running _ | Killed -> false
      ;;
    end

    type 'a t =
      { mutable state : 'a State.t
      ; event : 'a Task.t
      ; here : Source_code_position.t
      ; trigger : Trigger.t
      }

    type packed = T : 'a t -> packed [@@unboxed]

    let create ~here ~trigger computation =
      let event = Task.create () in
      let state = State.Unstarted computation in
      { event; state; here; trigger }
    ;;
  end

  type t =
    { children : Thunk_evaluator.packed Vec.t
    ; mutable step_number : int
    }

  let create () = { children = Vec.create (); step_number = 0 }

  (* Arbitrary number to make sure we don't retain the memory of dead tasks. *)
  let gc_tasks_interval = 1_024

  let rec handle_result
    : 'a.
    t
    -> 'a Thunk_evaluator.t
    -> ('a, unit) E.Result.t @ once unique
    -> (unit, Exn_and_backtrace.t) Result.t
    =
    fun (type a)
      (t : t)
      (thunk : a Thunk_evaluator.t)
      (result : (a, unit) E.Result.t @ once unique) ->
    match result with
    | Value a ->
      Task.resolve thunk.event a;
      thunk.state <- Finished a;
      Ok ()
    | Exception exn ->
      let backtrace = Backtrace.Exn.most_recent_for_exn exn in
      Error { Exn_and_backtrace.exn; backtrace }
    | Operation (op, k) ->
      (match op with
       | Step _ ->
         thunk.state <- Running (Unique.Once.make k);
         Ok ()
       | Spawn { created_at = here; computation = child_computation; trigger } ->
         let trigger =
           (* Inherits the trigger when unspecified *)
           match trigger with
           | None -> thunk.trigger
           | Some x -> x
         in
         let child = Thunk_evaluator.create ~here ~trigger child_computation in
         Vec.push_back t.children (T child);
         handle_result
           t
           thunk
           (Handled_effect.continue k { aliased_many = child.event } []))
  ;;

  let step_thunk t (triggers : Trigger.t iarray @ local) (thunk : _ Thunk_evaluator.t) =
    match Task.state thunk.event with
    | Killed ->
      thunk.state <- Killed;
      Ok ()
    | Resolved _ ->
      (* If the task state is resolved, the thunk must have been resolved. The thunk's
         completion is the only thing that can set a task to complete.
      *)
      assert (Thunk_evaluator.State.is_finished thunk.state);
      Ok ()
    | Pending ->
      (* In theory, we can make a hash table from [trigger -> Thunk evaluators], at the
         expense of determinism of the ordering of execution of tasks that are spawned (at
         least not without a much more complicated data structure) *)
      if Iarray.Local.exists triggers ~f:(fun trigger ->
           Trigger.is_equal trigger thunk.trigger)
      then (
        match thunk.state with
        | Unstarted computation -> handle_result t thunk (E.run computation)
        | Running k_uniq ->
          let k = Unique.Once.take_exn k_uniq in
          handle_result t thunk (Handled_effect.continue k () [])
        | Finished _ | Killed -> Ok ())
      else Ok ()
  ;;

  let add_task t ~created_at ~trigger computation =
    let thunk_evaluator = Thunk_evaluator.create ~here:created_at ~trigger computation in
    Vec.push_back t.children (T thunk_evaluator);
    thunk_evaluator.event
  ;;

  let step t triggers =
    (* It's necessary to use a while loop, rather than a for loop or Vec.iter, since the
       children themselves can spawn more children.

       We don't need to worry about t.children becoming shorter. Child tasks can only
       append tasks, but not remove them. *)
    let mutable i = 0 in
    let mutable children_exns = [] in
    while i < Vec.length t.children do
      let (T child) = Vec.get t.children i in
      (match step_thunk t triggers child with
       | Ok () -> ()
       | Error error -> children_exns <- (child.here, error) :: children_exns);
      i <- i + 1
    done;
    (match children_exns with
     | [] ->
       (* Special case this common case for performance *)
       ()
     | _ ->
       let all_exns = List.rev children_exns in
       (match all_exns with
        | [] -> ()
        | exns ->
          let exn_sexps =
            List.map exns ~f:(fun (spawned_at, exn_and_bt) ->
              let%tydi { exn; backtrace } = exn_and_bt in
              [%message
                (spawned_at : Source_code_position.t)
                  (exn : Exn.t)
                  (backtrace : Backtrace.t option)])
          in
          raise_s
            [%message
              "An exception occurred when stepping the LWS simulator. I have stepped all \
               the children."
                ~_:(exn_sexps : Sexp.t list)]));
    t.step_number <- t.step_number + 1;
    (* The first GC is triggered on the gc_tasks_interval-th (one-index) call to [step] *)
    if t.step_number mod gc_tasks_interval = 0
    then
      Vec.Inplace.filter t.children ~f:(fun (T child) ->
        match child.state with
        | Unstarted _ | Running _ -> true
        | Finished _ | Killed -> false)
  ;;

  let step_number t = t.step_number
end

let step ~created_at (h : Handler.t @ local) = E.perform h (Step created_at)

let spawn ~created_at ~trigger (h : Handler.t @ local) computation =
  let%tydi { aliased_many } = E.perform h (Spawn { created_at; computation; trigger }) in
  aliased_many
;;
