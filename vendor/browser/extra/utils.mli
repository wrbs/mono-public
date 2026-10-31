open Browser_types
open Browser_js_types

(** [add_event_listener_with_cleanup] add the provided listener to the [EventTarget] and
    returns a cleanup function that removes the event listener when called. *)
val add_event_listener_with_cleanup
  :  ?use_capture:bool
  -> #event_target Js.t
  -> type_:Js.js_string Js.t
  -> listener:('a -> event:event Js.t -> unit)
  -> event_id

(** [remove_event_listener] invokes the cleanup function returned by the
    [add_event_listener_with_cleanup] *)
val remove_event_listener : event_id -> unit
