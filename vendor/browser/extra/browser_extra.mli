open Browser_types
open Browser_js_types

(** This library provides utility functions that complement the bindings in the [Browser]
    and [Browser_expert] libraries. *)

(** [Coercion] provides utility functions for common coercions known to be equivalent.
    Please note that some of these coercions are possibly unsafe if a subclass overrides
    an ancestor class method with a completely different return type *)
module Coercion = Coercion

(** [Event_type] enumerates all *known event types allow with the interface(s) they use
    and the event targets that support them.

    *This list is **not** comprehensize. There may be mising event types, event
    interfaces, or event target interfaces *)
module Event_type = Event_type

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
