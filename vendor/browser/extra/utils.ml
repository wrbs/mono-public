open Browser_types
open Browser_js_types

let add_event_listener_with_cleanup
  ?use_capture
  (target : #event_target Js.t)
  ~type_
  ~listener
  =
  let listener = Js.Opt.return (Js.wrap_meth_callback listener) in
  let get_options ~to_js =
    match use_capture with
    | Some use_capture -> Js.bool use_capture |> to_js |> Js.Optdef.return
    | None -> Js.undefined
  in
  target##addEventListener
    ~type_
    ~listener
    ~options:(get_options ~to_js:bool_to_add_event_listener_options_or_bool);
  to_event_id (fun () ->
    target##removeEventListener
      ~type_
      ~listener
      ~options:(get_options ~to_js:bool_to_event_listener_options_or_bool))
;;

let remove_event_listener = remove_event_listener
