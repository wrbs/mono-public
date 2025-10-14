open! Core
open! Async
open! Bonsai_synth
open Bonsai.Let_syntax

let example_command name computation =
  let command =
    Command.async_or_error ~summary:[%string "Compute the %{name} example wav"]
    @@
    let%map_open.Command filename = anon ("OUTPUT" %: string) in
    fun () -> Driver.render_to_wav_file ~filename computation
  in
  name, command
;;

let mono_time value ~length ~stop_output graph =
  let on_startup =
    let%arr sleep = Uptime.sleep graph in
    let%bind.Effect () = sleep length in
    stop_output
  in
  Driver.Session.create [ value ] ~on_startup
;;

let play_sequence graph =
  (* TODO: this proves the concept but isn't actually accurate as any errors
  between block lengths and sleep times will accumulate making everything a bit
  slower than expected.

  Probably fix by doing something with [Uptime.until] relative to when the
  sequence gets fired so it's self correcting if timestamps drift, or make some
  custom thing that fires every tick dispatching events based on elapsed samples. *)
  let%arr sleep = Bonsai.Clock.sleep graph in
  fun sequence ~bpm ~handle_event ->
    let beat_secs = 60. /. bpm in
    List.fold_right
      sequence
      ~init:(Effect.return ())
      ~f:(fun (event, rest) rest_of_sequence ->
        Effect.Many
          [ handle_event event
          ; (let%bind.Effect () = sleep (Time_ns.Span.of_sec (beat_secs *. rest)) in
             rest_of_sequence)
          ])
;;

module Mono_sequence_event = struct
  type t =
    | Note of Note.t
    | Stop
    | Effect of unit Effect.t
end

let sin_sequence sequence graph =
  let envelope, send_envelope_event =
    Envelope.adsr
      ~decay:(Bonsai.return (Time_ns.Span.of_int_ms 500))
      ~sustain:(Bonsai.return 0.)
      graph
  in
  let note, set_note = Bonsai.state Note.middle_c graph in
  let output =
    match%sub envelope with
    | None -> Bonsai.return Block.zero
    | Some env ->
      let wave = Osc.sin ~freq:(note >>| Note.frequency >>| Block.const) graph in
      env *| wave
  in
  let start_sequence =
    let%arr play_sequence = play_sequence graph
    and set_note
    and send_envelope_event in
    play_sequence sequence ~bpm:120. ~handle_event:(function
      | Mono_sequence_event.Note note ->
        Effect.Many [ set_note note; send_envelope_event Start ]
      | Stop -> send_envelope_event Stop
      | Effect effect -> effect)
  in
  Driver.Session.create [ output ] ~on_startup:start_sequence
;;

let command =
  Command.group
    ~summary:"bonsai synth example"
    [ example_command "middle-c" (fun ~stop_output graph ->
        mono_time
          ~length:(Time_ns.Span.of_int_sec 2)
          ~stop_output
          (Osc.sin
             ~freq:(Bonsai.return (Note.middle_c |> Note.frequency |> Block.const))
             graph)
          graph)
    ; example_command "big-ben" (fun ~stop_output graph ->
        sin_sequence
          [ (* change 2 *)
            Note (E, 4), 1.
          ; Note (GS, 4), 1.
          ; Note (FS, 4), 1.
          ; Note (B, 3), 2.
          ; (* change 3 *)
            Note (E, 4), 1.
          ; Note (FS, 4), 1.
          ; Note (GS, 4), 1.
          ; Note (E, 4), 2.
          ; (* change 4 *)
            Note (GS, 4), 1.
          ; Note (E, 4), 1.
          ; Note (FS, 4), 1.
          ; Note (B, 3), 2.
          ; (* change 5 *)
            Note (B, 3), 1.
          ; Note (FS, 4), 1.
          ; Note (GS, 4), 1.
          ; Note (E, 4), 2.
          ; Stop, 5.
          ; (* bongs, 3-o-clock *)
            Note (E, 3), 4.
          ; Note (E, 3), 4.
          ; Note (E, 3), 4.
          ; Stop, 1.
          ; Effect stop_output, 0.
          ]
          graph)
    ]
;;

let run () = Command_unix.run command
