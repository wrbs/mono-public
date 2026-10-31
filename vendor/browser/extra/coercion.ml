open Js_of_ocaml
open Browser_types

(* Console *)
let to_jsoo_console (console : console Js.t) =
  let console : Console.console Js.t = Js.Unsafe.coerce console in
  console
;;

let of_jsoo_console (console : Console.console Js.t) =
  let console : console Js.t = Js.Unsafe.coerce console in
  console
;;

(* HTML Element *)
let to_jsoo_html_element (element : #html_element Js.t) =
  let element : Dom_html.element Js.t = Js.Unsafe.coerce element in
  element
;;

let of_jsoo_html_element (element : #Dom_html.element Js.t) =
  let element : html_element Js.t = Js.Unsafe.coerce element in
  element
;;

(* File List *)
let to_jsoo_file_list (file_list : file_list Js.t) =
  let file_list : File.fileList Js.t = Js.Unsafe.coerce file_list in
  file_list
;;

let of_jsoo_file_list (file_list : File.fileList Js.t) =
  let file_list : file_list Js.t = Js.Unsafe.coerce file_list in
  file_list
;;

(* Event *)
let to_jsoo_event (event : #event Js.t) =
  let event : Dom_html.event Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_event (event : #Dom_html.event Js.t) =
  let event : event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Focus Event *)
let to_jsoo_focus_event (event : focus_event Js.t) =
  let event : Dom_html.focusEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_focus_event (event : Dom_html.focusEvent Js.t) =
  let event : focus_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Mouse Event *)
let to_jsoo_mouse_event (event : mouse_event Js.t) =
  let event : Dom_html.mouseEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_mouse_event (event : Dom_html.mouseEvent Js.t) =
  let event : mouse_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Keyboard Event *)
let to_jsoo_keyboard_event (event : keyboard_event Js.t) =
  let event : Dom_html.keyboardEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_keyboard_event (event : Dom_html.keyboardEvent Js.t) =
  let event : keyboard_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Wheel Event *)
let to_jsoo_wheel_event (event : wheel_event Js.t) =
  let event : Dom_html.wheelEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_wheel_event (event : Dom_html.wheelEvent Js.t) =
  let event : wheel_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Mousewheel Event - maps to wheel_event *)
let to_jsoo_mousewheel_event (event : wheel_event Js.t) =
  let event : Dom_html.mousewheelEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_mousewheel_event (event : Dom_html.mousewheelEvent Js.t) =
  let event : wheel_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Touch Event *)
let to_jsoo_touch_event (event : touch_event Js.t) =
  let event : Dom_html.touchEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_touch_event (event : Dom_html.touchEvent Js.t) =
  let event : touch_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Submit Event *)
let to_jsoo_submit_event (event : submit_event Js.t) =
  let event : Dom_html.submitEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_submit_event (event : Dom_html.submitEvent Js.t) =
  let event : submit_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Drag Event *)
let to_jsoo_drag_event (event : drag_event Js.t) =
  let event : Dom_html.dragEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_drag_event (event : Dom_html.dragEvent Js.t) =
  let event : drag_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Clipboard Event *)
let to_jsoo_clipboard_event (event : clipboard_event Js.t) =
  let event : Dom_html.clipboardEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_clipboard_event (event : Dom_html.clipboardEvent Js.t) =
  let event : clipboard_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Toggle Event *)
let to_jsoo_toggle_event (event : toggle_event Js.t) =
  let event : Dom_html.toggleEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_toggle_event (event : Dom_html.toggleEvent Js.t) =
  let event : toggle_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Pop State Event *)
let to_jsoo_pop_state_event (event : pop_state_event Js.t) =
  let event : Dom_html.popStateEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_pop_state_event (event : Dom_html.popStateEvent Js.t) =
  let event : pop_state_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Pointer Event *)
let to_jsoo_pointer_event (event : pointer_event Js.t) =
  let event : Dom_html.pointerEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_pointer_event (event : Dom_html.pointerEvent Js.t) =
  let event : pointer_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Storage Event *)
let to_jsoo_storage_event (event : storage_event Js.t) =
  let event : Dom_html.storageEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_storage_event (event : Dom_html.storageEvent Js.t) =
  let event : storage_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Hash Change Event *)
let to_jsoo_hash_change_event (event : hash_change_event Js.t) =
  let event : Dom_html.hashChangeEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_hash_change_event (event : Dom_html.hashChangeEvent Js.t) =
  let event : hash_change_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Animation Event *)
let to_jsoo_animation_event (event : animation_event Js.t) =
  let event : Dom_html.animationEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_animation_event (event : Dom_html.animationEvent Js.t) =
  let event : animation_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Transition Event *)
let to_jsoo_transition_event (event : transition_event Js.t) =
  let event : Dom_html.transitionEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_transition_event (event : Dom_html.transitionEvent Js.t) =
  let event : transition_event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Media Event *)
let to_jsoo_media_event (event : event Js.t) =
  let event : Dom_html.mediaEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_media_event (event : Dom_html.mediaEvent Js.t) =
  let event : event Js.t = Js.Unsafe.coerce event in
  event
;;

(* Message Event *)
let to_jsoo_message_event (event : message_event Js.t) =
  let event : Dom_html.messageEvent Js.t = Js.Unsafe.coerce event in
  event
;;

let of_jsoo_message_event (event : Dom_html.messageEvent Js.t) =
  let event : message_event Js.t = Js.Unsafe.coerce event in
  event
;;
