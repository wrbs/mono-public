open! Core
open! Bonsai_web.Cont
open! Bonsai.Let_syntax

let text_input graph =
  let text_contents, set_text_contents =
    Bonsai.state_opt graph ~sexp_of_model:[%sexp_of: String.t] ~equal:[%equal: String.t]
  in
  let%arr text_contents = text_contents
  and set_text_contents = set_text_contents in
  Vdom_input_widgets.Entry.text
    ~merge_behavior:Legacy_dont_merge
    ~value:text_contents
    ~allow_updates_when_focused:`Never
    ~on_input:set_text_contents
    ()
;;

let date_input graph =
  let date_contents, set_date_contents =
    Bonsai.state_opt graph ~sexp_of_model:[%sexp_of: Date.t] ~equal:[%equal: Date.t]
  in
  let%arr date_contents = date_contents
  and set_date_contents = set_date_contents in
  Vdom_input_widgets.Entry.date
    ~merge_behavior:Legacy_dont_merge
    ~value:date_contents
    ~allow_updates_when_focused:`Never
    ~on_input:set_date_contents
    ()
;;

let text_input_first_input graph =
  let text_input_first_contents, set_text_input_first_contents =
    Bonsai.state false ~sexp_of_model:[%sexp_of: Bool.t] ~equal:[%equal: Bool.t] graph
  in
  let%arr text_input_first_contents = text_input_first_contents
  and set_text_input_first_contents = set_text_input_first_contents in
  ( text_input_first_contents
  , Vdom_input_widgets.Checkbox.simple
      ~merge_behavior:Legacy_dont_merge
      ~is_checked:text_input_first_contents
      ~label:
        {| When checked, the text input will be placed first in the DOM. Entry some
         data into both inputs below and check this box - the data in both boxes
         should be preserved. If this fails, check the console for an error message.
         It should be noted that this example demonstrates a bug in the virtual-dom
         library. |}
      ~on_toggle:(set_text_input_first_contents (not text_input_first_contents))
      () )
;;

let wrap_in_div nodes = nodes |> Bonsai.all >>| Vdom.Node.div

let component graph =
  let text_input = text_input graph in
  let date_input = date_input graph in
  let%sub text_input_first, text_input_first_input = text_input_first_input graph in
  if%sub text_input_first
  then wrap_in_div [ text_input_first_input; text_input; date_input ]
  else wrap_in_div [ text_input_first_input; date_input; text_input ]
;;

let () = Bonsai_web.Start.start component
