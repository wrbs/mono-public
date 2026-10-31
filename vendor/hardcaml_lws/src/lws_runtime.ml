module type S = Lws_runtime_intf.S

module Methods = struct
  type ('context, 'impl) t =
    { schedule_task :
        'a.
        ?trigger:Lws.Trigger.t
        -> here:[%call_pos]
        -> 'impl
        -> (Lws.Handler.t @ local -> 'context -> 'a)
        -> 'a Lws.Task.t
    ; poll_task : 'a. 'impl -> 'a Lws.Task.t -> 'a
    ; run :
        'a.
        ?trigger:Lws.Trigger.t
        -> here:[%call_pos]
        -> 'impl
        -> (Lws.Handler.t @ local -> 'context -> 'a)
        -> 'a
    ; context : 'impl -> 'context
    }
end

type 'context packed = T : ('context, 'impl) Methods.t * 'impl -> 'context packed
type 'context t = 'context packed constraint 'context = ('i, 'o) Lws_context.t

module To_polymorphic (X : S) = struct
  let to_polymorphic (type i o) (t : (i, o) Lws_context.t X.t) =
    let open X in
    T ({ schedule_task; poll_task; context; run }, t)
  ;;
end

let schedule_task ?trigger ~(here : [%call_pos]) (T (m, t') : _ t) thunk =
  m.schedule_task ?trigger ~here t' thunk
;;

let poll_task (T (m, t') : _ t) event = m.poll_task t' event

let run ?trigger ~(here : [%call_pos]) (T (m, t') : _ t) thunk =
  m.run ?trigger ~here t' thunk
;;

let context (T (m, t') : _ t) = m.context t'

let resolve_trigger_by_clock_name (t : _ t) name =
  (* We can't use dynamic dispatch here, so we end up with some code duplication. This is
     because [resolve_trigger_by_clock_name] only really works with the underlying
     'context is a Sim_ports.t *)
  Lws_context.resolve_trigger_by_clock_port_name (context t) name
;;
