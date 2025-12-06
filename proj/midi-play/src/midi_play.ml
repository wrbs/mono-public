open! Core
open! Async

let default_tempo = 500000

let merge_tracks (midi_file : Midi_file.t) =
  let v =
    midi_file.tracks
    |> Collection.iarray
    |> Collection.concat_map ~f:(fun track ->
      track
      |> Collection.iarray
      |> Collection.folding_map ~init:0 ~f:(fun tick { delta; kind } ->
        let tick = tick + Midi_file.U28.to_int delta in
        tick, (tick, kind)))
    |> Collector.(collect (vec ()))
  in
  Vec.sort v ~compare:[%compare: int * _];
  Collection.vec v
;;

let get_messages contents =
  let midi_file = Midi_file.of_string contents |> Or_error.ok_exn in
  let ticks_per_beat =
    match midi_file.header.timing with
    | Timecode _ -> failwith "timecode timing not supported"
    | Metrical n -> Midi_file.U15.to_int n
  in
  let get_s_per_tick ms_per_beat =
    Int.to_float ms_per_beat /. Int.to_float ticks_per_beat /. 1_000_000.
  in
  let messages = Queue.create () in
  let _ =
    merge_tracks midi_file
    |> Collection.fold
         ~init:(~t:0., ~s_per_tick:(get_s_per_tick default_tempo), ~last_tick:0)
         ~f:(fun (~t, ~s_per_tick, ~last_tick) (tick, kind) ->
           let delta = tick - last_tick in
           let t = t +. (Int.to_float delta *. s_per_tick) in
           let s_per_tick =
             match kind with
             | Meta (Tempo new_tempo) -> get_s_per_tick (Midi_file.U24.to_int new_tempo)
             | _ -> s_per_tick
           in
           let () =
             match kind with
             | MIDI msg -> Queue.enqueue messages (t, Midi.Live_message.MIDI msg)
             | _ -> ()
           in
           ~t, ~s_per_tick, ~last_tick:tick)
  in
  messages
;;

let playback queue ~port =
  let%bind ipmidi = Ipmidi.Port_sender.create port in
  Shutdown.at_shutdown (fun () ->
    Ipmidi.Port_sender.write_and_flush_exn ipmidi (Midi.Live_message.panic ());
    return ());
  let t0 = Time_ns.now () in
  let now () = Time_ns.diff (Time_ns.now ()) t0 |> Time_ns.Span.to_sec in
  let rec flush_pending ~cur_time =
    match Queue.peek_or_null queue with
    | Null -> `Finished
    | This (event_time, message) ->
      (match Float.O.(event_time <= cur_time + 0.001) with
       | true ->
         Queue.dequeue_and_ignore_exn queue;
         Ipmidi.Port_sender.write_exn ipmidi message;
         flush_pending ~cur_time
       | false -> `Next_event event_time)
  and write_then_flush ~cur_time =
    let result = flush_pending ~cur_time in
    Ipmidi.Port_sender.flush_exn ipmidi;
    match result with
    | `Finished -> return ()
    | `Next_event next_time ->
      let actual_time = now () in
      if Float.O.(next_time <= actual_time)
      then write_then_flush ~cur_time
      else (
        let next_wakeup = Time_ns.add t0 (Time_ns.Span.of_sec next_time) in
        let%bind () = Clock_ns.at next_wakeup in
        if Shutdown.is_shutting_down ()
        then return ()
        else write_then_flush ~cur_time:(now ()))
  in
  write_then_flush ~cur_time:(now ())
;;

let command =
  Command.async ~summary:"Play back a midi file"
  @@
  let%map_open.Command port = Ipmidi.Port.param ()
  and midi_file = anon ("MIDI_FILE" %: string) in
  fun () ->
    Signal.handle [ Signal.term; Signal.int ] ~f:(fun _ -> Shutdown.shutdown 0);
    Shutdown.shutdown_on_unhandled_exn ();
    let%bind contents = Reader.file_contents midi_file in
    let messages = get_messages contents in
    playback messages ~port
;;

let run () = Command_unix.run command
