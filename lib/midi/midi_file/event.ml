open! Core

type hexdump = string [@@deriving quickcheck, compare ~localize, equal ~localize]

let sexp_of_hexdump = String.Hexdump.sexp_of_t

module Meta = struct
  type t =
    | Sequence_number of Int_repr.Uint16.t option
    | Text of string
    | Copyright of string
    | Track_name of string
    | Instrument_name of string
    | Lyric of string
    | Marker of string
    | Cue_point of string
    | Program_name of string
    | Device_name of string
    | Midi_channel of Midi.Channel.t
    | Midi_port of Midi.Value.t
    | End_of_track
    | Tempo of Num.U24.t
    | Smtpe_offset of Smtpe.Time.t
    | Time_signature of Byte.t * Byte.t * Byte.t * Byte.t
    | Key_signature of Byte.t * Byte.t (* todo: proper validating *)
    | Sequencer_specific of hexdump
    | Unknown of Byte.t * hexdump
  [@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

  let read ~next =
    let open Io.Read in
    let open Result.Let_syntax in
    (* http://www.somascape.org/midi/tech/mfile.html#meta *)
    let%bind type_ = byte ~next in
    let%bind len = variable_length ~next >>| Num.U28.to_int in
    let text f =
      let%map data = string ~len ~next in
      f data
    in
    let reject bytes = Unknown (type_, String.of_char_list bytes) in
    match type_ with
    | '\x00' when len = 0 -> Ok (Sequence_number None)
    | '\x00' when len = 2 ->
      let%map n = u16 ~next in
      Sequence_number (Some (Int_repr.Uint16.of_base_int_exn n))
    | '\x01' -> text (fun s -> Text s)
    | '\x02' -> text (fun s -> Copyright s)
    | '\x03' -> text (fun s -> Track_name s)
    | '\x04' -> text (fun s -> Instrument_name s)
    | '\x05' -> text (fun s -> Lyric s)
    | '\x06' -> text (fun s -> Marker s)
    | '\x07' -> text (fun s -> Cue_point s)
    | '\x08' -> text (fun s -> Program_name s)
    | '\x09' -> text (fun s -> Device_name s)
    | '\x20' when len = 1 ->
      let%map b = byte ~next in
      (match Midi.Channel.of_int (Byte.to_int b) with
       | Some ch -> Midi_channel ch
       | None -> reject [ b ])
    | '\x21' when len = 1 ->
      let%map b = byte ~next in
      (match Midi.Value.of_byte b with
       | Some v -> Midi_port v
       | None -> reject [ b ])
    | '\x2F' when len = 0 -> Ok End_of_track
    | '\x51' when len = 3 ->
      let%map tempo = tempo_3_byte ~next in
      Tempo tempo
    | '\x54' when len = 5 ->
      let%bind hr = byte ~next in
      let%bind mn = byte ~next in
      let%bind se = byte ~next in
      let%bind fr = byte ~next in
      let%map ff = byte ~next in
      (match Smtpe.Time.of_bytes ~hr ~mn ~se ~fr ~ff with
       | None -> reject [ hr; mn; se; fr; ff ]
       | Some tc -> Smtpe_offset tc)
    | '\x58' when len = 4 ->
      let%bind n = byte ~next in
      let%bind d = byte ~next in
      let%bind c = byte ~next in
      let%map b = byte ~next in
      Time_signature (n, d, c, b)
    | '\x59' when len = 2 ->
      let%bind k = byte ~next in
      let%map b = byte ~next in
      Key_signature (k, b)
    | _ -> text (fun data -> Unknown (type_, data))
  ;;
end

module Kind = struct
  type t =
    | MIDI of Midi.Message.With_channel.t
    | Sysex of hexdump
    | Escape of hexdump
    | Meta of Meta.t
  [@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

  let read_message ~status:(channel, kind) ~(v1 : Midi.Value.t) ~next =
    let open Io.Read in
    let open Result.Let_syntax in
    let%map message =
      let (T kind) = Midi.Message.Kind.to_typed kind in
      match kind with
      | Single _ -> Ok (Midi.Message.decode_payload kind v1)
      | Double _ ->
        let%map v2 = value ~next in
        Midi.Message.decode_payload kind (v1, v2)
    in
    channel, message
  ;;

  let read_sysex ~next =
    let open Io.Read in
    let open Result.Let_syntax in
    let%bind len = variable_length ~next in
    string ~len:(Num.U28.to_int len) ~next
  ;;

  let read ~running_status ~next =
    let open Io.Read in
    let open Result.Let_syntax in
    let%bind b = byte ~next in
    let%map t, ~next_status =
      match Midi.Value.of_byte b with
      | Some v1 ->
        (match running_status with
         | None ->
           Or_error.error_s
             [%message "Unexpected value without running status set" (b : Byte.t)]
         | Some status ->
           let%map message = read_message ~status ~v1 ~next in
           MIDI message, ~next_status:(Some status))
      | None ->
        (match Midi.Message.Status.of_byte b with
         | Some status ->
           let%bind v1 = value ~next in
           let%map message = read_message ~status ~v1 ~next in
           MIDI message, ~next_status:(Some status)
         | _ ->
           (match b with
            | '\xFF' ->
              let%map meta = Meta.read ~next in
              Meta meta, ~next_status:None
            | '\xF0' ->
              let%map data = read_sysex ~next in
              Sysex data, ~next_status:None
            | '\xF7' ->
              let%map data = read_sysex ~next in
              Escape data, ~next_status:None
            | _ -> Or_error.error_s [%message "Unexpected event byte" (b : Byte.t)]))
    in
    t, ~running_status:next_status
  ;;
end

type t =
  { delta : Num.U28.t
  ; kind : Kind.t
  }
[@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

let read ~running_status ~next =
  let open Io.Read in
  let open Result.Let_syntax in
  let%bind delta = variable_length ~next in
  let%map kind, ~running_status = Kind.read ~running_status ~next in
  { delta; kind }, ~running_status
;;
