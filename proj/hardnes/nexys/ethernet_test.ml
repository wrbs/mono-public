open! Core
open! Hardcaml
open! Signal

(* A little test to try *)

open struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Uart = Uart
  module Uart_types = Uart_types
  module Clock_utils = Clock_utils
end

module Ethernet_clock = struct
  type 'a t =
    { clock_50 : 'a
    ; clear_50 : 'a
    }
  [@@deriving hardcaml]

  module BUFR_COMP = Clock_utils.BUFR.Make (struct
      include Clock_utils.BUFR.P

      let bufr_divide = "2"
    end)

  let generate_clear clock_50 reset_n =
    let reset_chain =
      reg_fb
        (Reg_spec.create ~clock:clock_50 ~reset:reset_n ~reset_edge:Falling ())
        ~reset_to:(Bits.ones 16)
        ~width:16
        ~f:(fun d -> sll d ~by:1)
    in
    reg (Reg_spec.create ~clock:clock_50 ()) (msb reset_chain)
  ;;

  let create scope ~(clocking : _ Nexys.Clock_and_reset.I.t) ~clear_100 =
    let clock_inputs =
      BUFR_COMP.I.{ ce = vdd; clr = clear_100; i = clocking.clock_100 }
    in
    let%tydi { o = clock_50 } = BUFR_COMP.create clock_inputs in
    let%hw clear_50 = generate_clear clock_50 clocking.reset_n in
    { clock_50; clear_50 }
  ;;
end

let create () =
  let board = Board.create () in
  let clocking = Nexys.Clock_and_reset.create board in
  let _clear = Utils.generate_clear clocking in
  board
;;
