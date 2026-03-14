open! Core
open! Hardcaml

type 'a t =
  { data : 'a
  ; valid : 'a
  ; ready : 'a
  }
[@@deriving hardcaml]

val create
  :  Hardcaml_hobby_boards.Board.t
  -> clocking:Signal.t Clocking.t
  -> clock_hz:int
  -> host_uart:Signal.t t * wire_board_uart:(Signal.t t -> unit)
