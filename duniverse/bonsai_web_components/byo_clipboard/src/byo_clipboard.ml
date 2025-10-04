open! Core
open! Bonsai_web
open! Bonsai.Let_syntax

let html_blob value =
  [ Js_of_ocaml.File.blob_from_string ~contentType:"text/html" value
  ; Js_of_ocaml.File.blob_from_string ~contentType:"text/plain" value
  ]
;;

let link_blob ~url ~title =
  [ Js_of_ocaml.File.blob_from_string ~contentType:"text/plain" url
  ; Js_of_ocaml.File.blob_from_string
      ~contentType:"text/html"
      (* Note: the surrounding <div> is required for Windows support.  *)
      [%string "<div><a href=\"%{url}\">%{title}</a></div>"]
  ]
;;

let copy_text value =
  let%map.Effect (_ : unit Or_error.t) =
    Js_clipboard.Asynchronous.copy_text (Js_of_ocaml.Js.string value)
  in
  ()
;;

let copy_html value =
  let%map.Effect (_ : unit Or_error.t) =
    Js_clipboard.Asynchronous.copy_blob (html_blob value)
  in
  ()
;;

let copy_link ~url ~title =
  let%map.Effect (_ : unit Or_error.t) =
    Js_clipboard.Asynchronous.copy_blob (link_blob ~url ~title)
  in
  ()
;;

module With_status = struct
  type t =
    [ `Idle of unit Effect.t
    | `Copied
    ]

  let temporary_toggle ~base ~temporary timeout (local_ graph) =
    let last_set_time, set_time =
      Bonsai.state
        Time_ns.min_value_representable
        ~equal:[%equal: Time_ns.Alternate_sexp.t]
        graph
    in
    let toggle_back_time =
      let%arr last_set_time in
      Time_ns.add last_set_time timeout
    in
    let toggle = Bonsai.Clock.at toggle_back_time graph in
    let get_now = Bonsai.Clock.get_current_time graph in
    let%arr set_time and toggle and get_now in
    let output =
      match toggle with
      | Before -> temporary
      | After -> base
    in
    let turn_on =
      let%bind.Effect now = get_now in
      set_time now
    in
    output, turn_on
  ;;

  let with_confirmation copy value (local_ graph) =
    let sleep = Bonsai.Clock.sleep graph in
    let temporary_toggle =
      temporary_toggle ~base:`Idle ~temporary:`Copied Time_ns.Span.second graph
    in
    let%arr sleep
    and state, toggle = temporary_toggle
    and value in
    match state with
    | `Idle ->
      let effect =
        let%bind.Effect (_ : unit Or_error.t) = copy value in
        let%bind.Effect () = toggle in
        sleep Time_ns.Span.second
      in
      `Idle effect
    | `Copied -> `Copied
  ;;

  let copy_text value (local_ graph) =
    let value =
      let%arr value in
      Js_of_ocaml.Js.string value
    in
    with_confirmation Js_clipboard.Asynchronous.copy_text value graph
  ;;

  let copy_html value (local_ graph) =
    let value = value >>| html_blob in
    with_confirmation Js_clipboard.Asynchronous.copy_blob value graph
  ;;

  let copy_link ~url ~title (local_ graph) =
    let value =
      let%arr url and title in
      link_blob ~url ~title
    in
    with_confirmation Js_clipboard.Asynchronous.copy_blob value graph
  ;;
end
