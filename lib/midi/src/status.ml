open! Core

module Realtime = struct
  type t =
    | Clock
    | Tick
    | Start
    | Continue
    | Stop
    | FD
    | Active_sense
    | Reset
  [@@deriving
    sexp_of, quickcheck ~portable, compare ~localize, equal ~localize, enumerate]
end

type t =
  | MIDI of Message.Status.t
  | Sysex
  | End_sysex
  | Mtc_quarter_frame
  | Song_position
  | Song_select
  | Tune_request
  | Realtime of Realtime.t
  | U_F4
  | U_F5
[@@deriving sexp_of, quickcheck ~portable, compare ~localize, equal ~localize]

let of_byte b =
  match b with
  | '\x00' .. '\x7F' -> None
  | '\x80' .. '\xEF' ->
    let midi_status = Message.Status.of_byte b |> Option.value_exn in
    Some (MIDI midi_status)
  | '\xF0' -> Some Sysex
  | '\xF1' -> Some Mtc_quarter_frame
  | '\xF2' -> Some Song_position
  | '\xF3' -> Some Song_select
  | '\xF4' -> Some U_F4
  | '\xF5' -> Some U_F5
  | '\xF6' -> Some Tune_request
  | '\xF7' -> Some End_sysex
  | '\xF8' -> Some (Realtime Clock)
  | '\xF9' -> Some (Realtime Tick)
  | '\xFA' -> Some (Realtime Start)
  | '\xFB' -> Some (Realtime Continue)
  | '\xFC' -> Some (Realtime Stop)
  | '\xFD' -> Some (Realtime FD)
  | '\xFE' -> Some (Realtime Active_sense)
  | '\xFF' -> Some (Realtime Reset)
;;

let of_byte_exn b =
  match of_byte b with
  | Some t -> t
  | None -> failwithf "invalid status byte: %s" (Byte.to_string b) ()
;;

let to_byte = function
  | MIDI midi_status -> Message.Status.to_byte midi_status
  | Sysex -> '\xF0'
  | End_sysex -> '\xF7'
  | Mtc_quarter_frame -> '\xF1'
  | Song_position -> '\xF2'
  | Song_select -> '\xF3'
  | Tune_request -> '\xF6'
  | Realtime rt ->
    (match rt with
     | Clock -> '\xF8'
     | Tick -> '\xF9'
     | Start -> '\xFA'
     | Continue -> '\xFB'
     | Stop -> '\xFC'
     | FD -> '\xFD'
     | Active_sense -> '\xFE'
     | Reset -> '\xFF')
  | U_F4 -> '\xF4'
  | U_F5 -> '\xF5'
;;

let all = Byte.range_incl_list '\x80' '\xFF' |> List.map ~f:of_byte_exn
