open! Core
open! Async

let panic_command =
  Command.async ~summary:"Sends a 'stop all notes' messages to every ipmidi port"
  @@
  let%map_open.Command ports =
    flag
      [%var_dash_name]
      (optional (Command.Arg_type.comma_separated Ipmidi.Port.arg_type))
      ~doc:"_ comma-separated list of ports to send panic to (default: all)"
  in
  fun () ->
    let panic =
      Midi.Channel.all
      |> Collection.list
      |> Collection.map ~f:(fun channel ->
        Midi.Live_message.cc
          Midi.Value.zero
          ~controller:(Midi.Value.of_int_exn 123)
          ~channel)
    in
    let%bind ipmidi = Ipmidi.Sender.create () in
    let ports = Option.value ports ~default:Ipmidi.Port.all in
    List.iter ports ~f:(fun port -> Ipmidi.Sender.write_and_flush_exn ipmidi panic ~port);
    return ()
;;

let channel_flag =
  Command.Param.flag_optional_with_default_doc_string
    "channel"
    Midi.Channel.arg_type
    Midi.Channel.to_string
    ~default:C1
    ~doc:"_ channel to send to"
;;

let send_note_command =
  Command.async ~summary:"Sends a 'note on' message. Velocity defaults to 0 (note off)"
  @@
  let%map_open.Command port = Ipmidi.Port.param ()
  and channel = channel_flag
  and note = anon ("NOTE" %: Midi.Value.arg_type)
  and velocity =
    anon (maybe_with_default Midi.Value.zero ("VELOCITY" %: Midi.Value.arg_type))
  in
  fun () ->
    let%bind ipmidi = Ipmidi.Port_sender.create port in
    Ipmidi.Port_sender.write_and_flush_exn
      ipmidi
      (Collection.singleton (Midi.Live_message.note_on note ~velocity ~channel));
    return ()
;;

let sexp_command =
  Command.async ~summary:"dump a midi file as sexp"
  @@
  let%map_open.Command file = anon ("FILE_PATH" %: string) in
  fun () ->
    let%map contents = Reader.file_contents file in
    let midi = Midi_file.of_string contents |> Or_error.ok_exn in
    print_s [%sexp (midi : Midi_file.t)]
;;

let command =
  Command.group
    ~summary:"sample midi stuff"
    [ "panic", panic_command; "send-note", send_note_command; "sexp", sexp_command ]
;;

let () = Command_unix.run command
