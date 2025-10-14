open! Core
open! Async
open! Import

module Wav_writer : sig
  type t

  val with_ : filename:string -> (t -> 'a Deferred.t) -> 'a Deferred.t
  val write_header : t -> sample_rate_hz:int -> num_channels:int -> unit
  val write_sample : t -> float -> unit
end = struct
  type t =
    { writer : Writer.t
    ; mutable sample_bytes_written : int
    }

  let sample_bytes = 2
  let write_string t s = Writer.write t.writer s

  let write_le_32' n ~write =
    let u32 =
      if n > 0
      then Int_repr.Uint32.of_base_int_exn n
      else (
        let i32 = Int_repr.Int32.of_base_int32 (Int32.of_int_exn n) in
        Int_repr.Uint32.of_int32_wrap i32)
    in
    let todo = Int_repr.Uint32.to_base_int_exn u32 in
    let b0, todo = todo land 0xFF, todo lsr 8 in
    let b1, todo = todo land 0xFF, todo lsr 8 in
    let b2, todo = todo land 0xFF, todo lsr 8 in
    let b3, todo = todo land 0xFF, todo lsr 8 in
    [%test_eq: int] todo 0;
    write (Char.of_int_exn b0);
    write (Char.of_int_exn b1);
    write (Char.of_int_exn b2);
    write (Char.of_int_exn b3)
  ;;

  let write_le_32 t n = write_le_32' n ~write:(fun c -> Writer.write_char t.writer c)

  let write_le_32_fd_blocking n ~fd =
    let buffer = Buffer.create 4 in
    write_le_32' n ~write:(Buffer.add_char buffer);
    let written =
      Core_unix.single_write
        (Core_unix.File_descr.of_int_exn (Fd.to_int_exn fd))
        ~buf:(Buffer.contents_bytes buffer)
    in
    [%test_eq: int] written 4
  ;;

  let write_le_16 t n =
    let u16 =
      if n > 0
      then Int_repr.Uint16.of_base_int_exn n
      else (
        let i16 = Int_repr.Int16.of_base_int_exn n in
        Int_repr.Uint16.of_int16_wrap i16)
    in
    let todo = Int_repr.Uint16.to_base_int u16 in
    let b0, todo = todo land 0xFF, todo lsr 8 in
    let b1, todo = todo land 0xFF, todo lsr 8 in
    [%test_eq: int] todo 0;
    Writer.write_char t.writer (Char.of_int_exn b0);
    Writer.write_char t.writer (Char.of_int_exn b1)
  ;;

  let write_header t ~sample_rate_hz ~num_channels =
    (* header + space for chunk size *)
    write_string t "RIFF";
    write_le_32 t 0 (* space for file size, filled later *);
    write_string t "WAVE";
    let () =
      (* write header *)
      write_string t "fmt ";
      write_le_32 t 16 (* fmt length *);
      write_le_16 t 0x0001 (* PCM*);
      write_le_16 t num_channels;
      write_le_32 t sample_rate_hz;
      let avg_bytes_per_second = sample_rate_hz * sample_bytes * num_channels in
      write_le_32 t avg_bytes_per_second;
      let block_align = sample_bytes * num_channels in
      write_le_16 t block_align;
      let bits_per_sample = 8 * sample_bytes in
      write_le_16 t bits_per_sample
    in
    (* start data chunk *)
    write_string t "data";
    write_le_32 t 0
  ;;

  let file_size_offset = 4
  let data_chunk_size_offset = 8 (* riff header *) + 24 (* midi header *)

  let write_sample t sample =
    sample *. 32767.
    |> Float.clamp_exn ~min:(-32768.) ~max:32767.
    |> Float.to_int
    |> write_le_16 t;
    t.sample_bytes_written <- t.sample_bytes_written + 2
  ;;

  let fix_lengths ~filename ~sample_bytes_written =
    let%bind fd = Unix.openfile filename ~mode:[ `Rdwr ] in
    let%bind (_ : int64) = Unix.lseek fd (Int64.of_int file_size_offset) ~mode:`Cur in
    write_le_32_fd_blocking (4 + 24 + (sample_bytes_written * sample_bytes)) ~fd;
    let%bind (_ : int64) =
      Unix.lseek fd (Int64.of_int data_chunk_size_offset) ~mode:`Cur
    in
    write_le_32_fd_blocking (sample_bytes_written * sample_bytes) ~fd;
    Unix.close fd
  ;;

  let with_ ~filename f =
    let%bind result, sample_bytes_written =
      Writer.with_file filename ~f:(fun writer ->
        let t = { writer; sample_bytes_written = 0 } in
        let%map result = f t in
        result, t.sample_bytes_written)
    in
    let%map () = fix_lengths ~filename ~sample_bytes_written in
    result
  ;;
end

let mock_time ~sample_rate_hz ~num_samples =
  Time_ns.add
    Time_ns.epoch
    (Time_ns.Span.of_sec (Float.of_int num_samples /. Float.of_int sample_rate_hz))
;;

module Action = struct
  type t =
    | Startup of unit Effect.t
    | Tick of (unit -> unit)
    | Stop
end

module Session = struct
  type t =
    { on_startup : unit Effect.t Bonsai.t
    ; channels : Block.t Bonsai.t list
    }

  let create ?(on_startup = Bonsai.return Effect.Ignore) channels =
    { on_startup; channels }
  ;;

  let wrap computation ~wav ~sample_rate_hz =
    let open Bonsai.Let_syntax in
    let stopped = Bonsai.Expert.Var.create false in
    Bonsai.Dynamic_scope.set
      Sample_rate.Expert.dynamic_scope
      (Bonsai.return sample_rate_hz)
      ~inside:(fun graph ->
        let t =
          computation
            ~stop_output:(Effect.of_thunk (fun () -> Bonsai.Expert.Var.set stopped true))
            graph
        in
        Wav_writer.write_header wav ~sample_rate_hz ~num_channels:(List.length t.channels);
        let is_initialized, set_initialized = Bonsai.state false graph in
        match%sub is_initialized with
        | false ->
          let%arr on_startup = t.on_startup
          and set_initialized in
          Action.Startup (Effect.Many [ on_startup; set_initialized true ])
        | true ->
          (match%sub Bonsai.Expert.Var.value stopped with
           | true -> return Action.Stop
           | false ->
             let%arr blocks = Bonsai.all t.channels in
             Action.Tick
               (fun () ->
                 for idx = 0 to Block.size - 1 do
                   List.iter blocks ~f:(fun block ->
                     let sample = Block.get block idx in
                     Wav_writer.write_sample wav sample)
                 done)))
  ;;
end

let render_to_wav_file ?(sample_rate_hz = 44_100) ~filename computation =
  Deferred.Or_error.try_with
  @@ fun () ->
  Wav_writer.with_ ~filename (fun wav ->
    let clock =
      Bonsai.Time_source.create ~start:(mock_time ~sample_rate_hz ~num_samples:0)
    in
    let driver =
      Bonsai_driver.create
        ~time_source:clock
        ~instrumentation:(Bonsai_driver.Instrumentation.default_for_test_handles ())
        (Session.wrap computation ~wav ~sample_rate_hz)
    in
    Deferred.repeat_until_finished 0 (fun num_samples ->
      Bonsai_driver.flush driver;
      match Bonsai_driver.result driver with
      | Stop -> return (`Finished ())
      | Startup effect ->
        Bonsai_driver.schedule_event driver effect;
        return (`Repeat 0)
      | Tick f ->
        f ();
        let num_samples = num_samples + Block.size in
        Bonsai.Time_source.advance_clock
          clock
          ~to_:(mock_time ~num_samples ~sample_rate_hz);
        Bonsai_driver.trigger_lifecycles driver;
        return (`Repeat num_samples)))
;;
