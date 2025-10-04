open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax

let component graph =
  let height, increase_height =
    Bonsai.state_machine0
      graph
      ~sexp_of_model:[%sexp_of: Int.t]
      ~equal:[%equal: Int.t]
      ~sexp_of_action:[%sexp_of: Unit.t]
      ~default_model:0
      ~apply_action:(fun (_ : _ Bonsai.Apply_action_context.t) old_model () ->
      old_model + 10)
  in
  let append_effect =
    let%arr height = height in
    Effect.of_sync_fun
      Inline_css.Private.append
      [%string {|
        .my_box {
          height: %{height#Int}px;
        }
      |}]
  in
  let effect =
    let%arr increase_height = increase_height
    and append_effect = append_effect in
    let%bind.Effect () = append_effect in
    increase_height ()
  in
  let%arr effect = effect in
  Vdom.Node.div
    [ Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.class_ "my_box"
          ; Vdom.Attr.style Css_gen.(background_color (`Hex "#000000") @> width (`Px 20))
          ]
        []
    ; Vdom.Node.button
        ~attrs:[ Vdom.Attr.on_click (fun _ -> effect) ]
        [ Vdom.Node.text "Append style!" ]
    ]
;;

let () = Bonsai_web.Start.start component
