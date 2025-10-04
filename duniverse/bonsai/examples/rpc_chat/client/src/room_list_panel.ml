open! Core
open! Async_kernel
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Bonsai_chat_common

module Style =
[%css
stylesheet
  {|
  .room_list_panel {
       width: 150px;
       display:flex;
       flex-direction:column;
   }

  .refresh_button {
      background:white;
      border:0;
  }
|}]

let component ~room_list ~refresh_rooms ~change_room _graph =
  let%arr room_list = room_list
  and refresh_rooms = refresh_rooms in
  let room_header =
    Vdom.Node.h2
      [ Vdom.Node.text "Rooms"
      ; Vdom_input_widgets.Button.simple
          ~merge_behavior:Legacy_dont_merge
          ~extra_attrs:[ Style.refresh_button ]
          ~on_click:(fun () -> refresh_rooms)
          "↻"
      ]
  in
  let room_switching_buttons =
    List.map room_list ~f:(fun room ->
      let on_click () = change_room room in
      Vdom_input_widgets.Button.simple
        ~merge_behavior:Legacy_dont_merge
        ~on_click
        (Room.to_string room))
  in
  Vdom.Node.div ~attrs:[ Style.room_list_panel ] ([ room_header ] @ room_switching_buttons)
;;
