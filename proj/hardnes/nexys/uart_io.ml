open! Core
open! Hardcaml
open! Signal

open struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Uart = Uart
  module Uart_types = Uart_types
end

module T = struct
  type 'a t =
    { data : 'a [@bits 8]
    ; valid : 'a
    ; ready : 'a
    }
  [@@deriving hardcaml]
end

let create board ~clocking ~clock_hz =
  let scope = Board.scope board in
  let uart = Nexys.Uart.create board in
  let%hw.T.Of_signal board_uart = T.Of_signal.wires () in
  let config : _ Uart.Config.t =
    { data_bits = Uart_types.Data_bits.Enum.Of_signal.of_enum Eight
    ; parity = Uart_types.Parity.Enum.Of_signal.of_enum Even
    ; stop_bits = Uart_types.Stop_bits.Enum.Of_signal.of_enum One
    ; clocks_per_bit =
        of_unsigned_int ~width:Uart.Config.port_widths.clocks_per_bit (clock_hz / 115_200)
    }
  in
  let uart_rx =
    Uart.Rx.create
      ~align_rxdata_to_lsb:true
      scope
      { clocking; enable = vdd; config; rxd = uart.rxd }
  in
  let uart_tx =
    Uart.Tx.create
      scope
      { clocking
      ; config
      ; data_in = gnd @: board_uart.data
      ; data_in_valid = board_uart.valid
      }
  in
  let%hw.T.Of_signal host_uart =
    { data = sel_bottom uart_rx.data_out ~width:8
    ; valid = uart_rx.data_out_valid
    ; ready = uart_tx.data_in_ready
    }
  in
  Nexys.Uart.complete board { rts = gnd; txd = uart_tx.txd };
  let wire_board_uart x = T.Of_signal.assign board_uart x in
  ~host_uart, ~wire_board_uart
;;

include T
