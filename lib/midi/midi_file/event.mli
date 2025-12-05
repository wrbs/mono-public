@@ portable

open! Core

module Meta : sig
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
    | Key_signature of Byte.t * Byte.t
    | Sequencer_specific of Byte.String.t
    | Unknown of Byte.t * Byte.String.t
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

  val read : t Io.Read.t
end

module Kind : sig
  type t =
    | MIDI of Midi.Message.With_channel.t
    | Sysex of Byte.String.t
      (* Probably should be [Midi.Value.t]s but the spec does not require *)
    | Escape of Byte.String.t
    | Meta of Meta.t
  [@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

  val read
    :  running_status:Midi.Message.Status.t option
    -> (t * running_status:Midi.Message.Status.t option) Io.Read.t
end

type t =
  { delta : Num.U28.t
  ; kind : Kind.t
  }
[@@deriving sexp_of, quickcheck, compare ~localize, equal ~localize]

val read
  :  running_status:Midi.Message.Status.t option
  -> (t * running_status:Midi.Message.Status.t option) Io.Read.t
