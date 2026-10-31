open Js_of_ocaml
open Browser_types

(** Coercion functions between js_of_ocaml Dom_html types and Browser types.

    Note that some of these conversions could be dangerous if the subclass has a method or
    attribute that overrides the ancestor class and returns a completely different type *)

(** Console conversions *)
val to_jsoo_console : console Js.t -> Console.console Js.t

val of_jsoo_console : Console.console Js.t -> console Js.t

(** HTML Element conversions *)
val to_jsoo_html_element : #html_element Js.t -> Dom_html.element Js.t

val of_jsoo_html_element : #Dom_html.element Js.t -> html_element Js.t

(** Event conversions *)
val to_jsoo_event : #event Js.t -> Dom_html.event Js.t

val of_jsoo_event : #Dom_html.event Js.t -> event Js.t

(** File List conversions *)
val to_jsoo_file_list : file_list Js.t -> File.fileList Js.t

val of_jsoo_file_list : File.fileList Js.t -> file_list Js.t

(** Focus Event conversions *)
val to_jsoo_focus_event : focus_event Js.t -> Dom_html.focusEvent Js.t

val of_jsoo_focus_event : Dom_html.focusEvent Js.t -> focus_event Js.t

(** Mouse Event conversions *)
val to_jsoo_mouse_event : mouse_event Js.t -> Dom_html.mouseEvent Js.t

val of_jsoo_mouse_event : Dom_html.mouseEvent Js.t -> mouse_event Js.t

(** Keyboard Event conversions *)
val to_jsoo_keyboard_event : keyboard_event Js.t -> Dom_html.keyboardEvent Js.t

val of_jsoo_keyboard_event : Dom_html.keyboardEvent Js.t -> keyboard_event Js.t

(** Wheel Event conversions *)
val to_jsoo_wheel_event : wheel_event Js.t -> Dom_html.wheelEvent Js.t

val of_jsoo_wheel_event : Dom_html.wheelEvent Js.t -> wheel_event Js.t

(** Mousewheel Event conversions (maps to wheel_event) *)
val to_jsoo_mousewheel_event : wheel_event Js.t -> Dom_html.mousewheelEvent Js.t

val of_jsoo_mousewheel_event : Dom_html.mousewheelEvent Js.t -> wheel_event Js.t

(** Touch Event conversions *)
val to_jsoo_touch_event : touch_event Js.t -> Dom_html.touchEvent Js.t

val of_jsoo_touch_event : Dom_html.touchEvent Js.t -> touch_event Js.t

(** Submit Event conversions *)
val to_jsoo_submit_event : submit_event Js.t -> Dom_html.submitEvent Js.t

val of_jsoo_submit_event : Dom_html.submitEvent Js.t -> submit_event Js.t

(** Drag Event conversions *)
val to_jsoo_drag_event : drag_event Js.t -> Dom_html.dragEvent Js.t

val of_jsoo_drag_event : Dom_html.dragEvent Js.t -> drag_event Js.t

(** Clipboard Event conversions *)
val to_jsoo_clipboard_event : clipboard_event Js.t -> Dom_html.clipboardEvent Js.t

val of_jsoo_clipboard_event : Dom_html.clipboardEvent Js.t -> clipboard_event Js.t

(** Toggle Event conversions *)
val to_jsoo_toggle_event : toggle_event Js.t -> Dom_html.toggleEvent Js.t

val of_jsoo_toggle_event : Dom_html.toggleEvent Js.t -> toggle_event Js.t

(** Pop State Event conversions *)
val to_jsoo_pop_state_event : pop_state_event Js.t -> Dom_html.popStateEvent Js.t

val of_jsoo_pop_state_event : Dom_html.popStateEvent Js.t -> pop_state_event Js.t

(** Pointer Event conversions *)
val to_jsoo_pointer_event : pointer_event Js.t -> Dom_html.pointerEvent Js.t

val of_jsoo_pointer_event : Dom_html.pointerEvent Js.t -> pointer_event Js.t

(** Storage Event conversions *)
val to_jsoo_storage_event : storage_event Js.t -> Dom_html.storageEvent Js.t

val of_jsoo_storage_event : Dom_html.storageEvent Js.t -> storage_event Js.t

(** Hash Change Event conversions *)
val to_jsoo_hash_change_event : hash_change_event Js.t -> Dom_html.hashChangeEvent Js.t

val of_jsoo_hash_change_event : Dom_html.hashChangeEvent Js.t -> hash_change_event Js.t

(** Animation Event conversions *)
val to_jsoo_animation_event : animation_event Js.t -> Dom_html.animationEvent Js.t

val of_jsoo_animation_event : Dom_html.animationEvent Js.t -> animation_event Js.t

(** Transition Event conversions *)
val to_jsoo_transition_event : transition_event Js.t -> Dom_html.transitionEvent Js.t

val of_jsoo_transition_event : Dom_html.transitionEvent Js.t -> transition_event Js.t

(** Media Event conversions *)
val to_jsoo_media_event : event Js.t -> Dom_html.mediaEvent Js.t

val of_jsoo_media_event : Dom_html.mediaEvent Js.t -> event Js.t

(** Message Event conversions *)
val to_jsoo_message_event : message_event Js.t -> Dom_html.messageEvent Js.t

val of_jsoo_message_event : Dom_html.messageEvent Js.t -> message_event Js.t
