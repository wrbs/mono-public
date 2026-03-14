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

  let create scope ~(nexys_clock : _ Nexys.Clock_and_reset.I.t) ~clear_100 : _ Clocking.t =
    let clock_inputs =
      BUFR_COMP.I.{ ce = vdd; clr = clear_100; i = nexys_clock.clock_100 }
    in
    let%tydi { o = clock } = BUFR_COMP.create clock_inputs in
    let%hw clear = generate_clear clock nexys_clock.reset_n in
    { clock; clear }
  ;;
end

let create () =
  let board = Board.create () in
  let scope = Board.scope board in
  let nexys_clock = Nexys.Clock_and_reset.create board in
  let clocking =
    let clear_100 = Utils.generate_clear nexys_clock in
    Ethernet_clock.create scope ~nexys_clock ~clear_100
  in
  let reg_spec = Clocking.to_spec clocking in
  let ~host_uart, ~wire_board_uart =
    Uart_io.create board ~clocking ~clock_hz:50_000_000
  in
  let%hw has_init = reg_fb reg_spec ~width:1 ~f:(fun prev -> prev |: host_uart.valid) in
  let%hw init = has_init &: ~:(reg reg_spec has_init) in
  let inputs = Nexys.Ethernet.create board in
  let%tydi { hw_tx; start; stop; abort } =
    Hardcaml_home_udp_arp.Demo.create
      scope
      { clocking
      ; init
      ; hw_rx =
          { crsdv = reg reg_spec inputs.crsdv
          ; rxerr = reg reg_spec inputs.rxerr
          ; rxd = reg reg_spec inputs.rxd
          }
      }
  in
  Nexys.Ethernet.complete
    board
    { txen = reg reg_spec hw_tx.txen
    ; txd = reg reg_spec hw_tx.txd
    ; rstn = nexys_clock.reset_n
    ; refclk = clocking.clock
    };
  (* let rising_crsdv = inputs.crsdv <>: reg reg_spec inputs.crsdv in *)
  let rising_txen = hw_tx.txen <>: reg reg_spec hw_tx.txen in
  let%hw_var uart_valid = Always.Variable.reg reg_spec ~width:1 in
  let%hw_var data = Always.Variable.reg reg_spec ~width:8 in
  Always.(
    compile
      [ if_ uart_valid.value [ when_ host_uart.ready [ uart_valid <-- gnd ] ]
        @@ else_
             [ when_ (has_init &: start) [ data <-- of_char 's'; uart_valid <-- vdd ]
             ; when_ (has_init &: stop) [ data <-- of_char 'S'; uart_valid <-- vdd ]
             ; when_ (has_init &: abort) [ data <-- of_char 'a'; uart_valid <-- vdd ]
             ; when_
                 (has_init &: rising_txen)
                 [ data <-- of_char 'T'; uart_valid <-- vdd ]
             ; when_ init [ data <-- of_char 'k'; uart_valid <-- vdd ]
             ]
      ]);
  wire_board_uart { data = data.value; ready = vdd; valid = uart_valid.value };
  board
;;
