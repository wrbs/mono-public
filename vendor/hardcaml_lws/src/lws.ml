open Core
open Hardcaml

include struct
  open Lws_core
  module Task = Task
end

module Before_and_after_edge = Before_and_after_edge
module Trigger = Lws_core.Trigger

module Config = struct
  type t =
    { timeout : int option
    ; auto_label_hierarchical_ports : bool
    ; assertions : bool
    }

  let default =
    { timeout = None; auto_label_hierarchical_ports = true; assertions = false }
  ;;
end

module Handler = struct
  type t = Lws_core.Handler.t
end

let step ~(here : [%call_pos]) ?(n = 1) (handle @ local) =
  if n < 0 then raise_s [%message "step must take 0 or more cycles" (n : int)];
  for _ = 1 to n do
    Lws_core.step handle ~created_at:here
  done
;;

let cycle = step

let spawn ?trigger ~(here : [%call_pos]) (handle @ local) thunk =
  Lws_core.spawn ~trigger ~created_at:here handle thunk
;;

let spawn_ignore_task ?trigger ~(here : [%call_pos]) (handle @ local) thunk =
  ignore (spawn ?trigger ~here handle thunk : _ Lws_core.Task.t)
;;

let spawn_unit = spawn_ignore_task
let spawn_never_returns = spawn_ignore_task

let wait ~(here : [%call_pos]) (handle @ local) (ev : _ Task.t) =
  while Task.is_pending ev do
    Lws_core.step handle ~created_at:here
  done;
  match Task.state ev with
  | Resolved a -> a
  | Killed -> raise_s [%message "Lws.wait: task was killed"]
  | Pending -> (* unreachable: the while loop ensures we are not pending *) assert false
;;

let rec step_until_this ~(here : [%call_pos]) (h @ local) f =
  match f () with
  | Null ->
    step ~here h;
    step_until_this ~here h f
  | This x -> x
;;

let rec step_until_some ~(here : [%call_pos]) (h @ local) f =
  match f () with
  | None ->
    step ~here h;
    step_until_some ~here h f
  | Some x -> x
;;
