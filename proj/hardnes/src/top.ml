open! Core
open! Hardcaml
open! Signal

let master_clock_hz = 21_477_270

open struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Utils = Utils
  module Nexys = Nexys_a7_100t
  module Uart = Uart
  module Uart_types = Uart_types
end

open struct
  module Cpu = Hardnes_rp2a03.Cpu
end

module Memory = struct
  module I = struct
    type 'a t =
      { enable : 'a
      ; addr : 'a [@bits 16]
      ; data_in : 'a [@bits 8]
      ; write : 'a
      ; clock : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t = { data : 'a [@bits 8] } [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let enable = i.enable in
    let reg_spec = Reg_spec.create ~clock:i.clock () in
    let%hw_var last_bus = Always.Variable.reg ~width:8 reg_spec ~enable in
    let%hw_var data_out = Always.Variable.wire ~default:last_bus.value () in
    let%hw ram_range = sel_top i.addr ~width:3 |> no_bits_set in
    let%hw ram_addr = sel_bottom i.addr ~width:11 in
    let%hw rom_range = i.addr.:(15) in
    let%hw rom_addr = sel_bottom i.addr ~width:14 in
    let rom_data =
      Sprom.create
        rom_addr
        ~width:8
        ~clock:i.clock
        ~enable:(i.enable &: rom_range)
        ~reset:gnd
        ~init_file:"prg-rom.mem"
    in
    let ram =
      Ram.create
        ~collision_mode:Read_before_write
        ~size:0x800
        ~write_ports:
          [| { write_clock = i.clock
             ; write_address = ram_addr
             ; write_enable = i.enable &: i.write &: ram_range
             ; write_data = i.data_in
             }
          |]
        ~read_ports:
          [| { read_enable = i.enable; read_address = ram_addr; read_clock = i.clock } |]
        ()
    in
    Always.(
      compile
        [ last_bus <-- data_out.value
        ; if_
            i.write
            [ last_bus <-- i.data_in ]
            [ when_ ram_range [ data_out <-- ram.(0) ]
            ; when_ rom_range [ data_out <-- rom_data ]
            ]
        ]);
    { data = data_out.value }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"memory" create input
  ;;
end

module Make_bcd (Digits : sig
    val num_digits : int
  end) =
struct
  open Digits

  (* Bits required to represent [999...999] *)
  let binary_bits = num_bits_to_represent (Int.pow 10 num_digits - 1)

  module State = struct
    type t =
      | Start
      | Double
      | Dabble
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  let create ~clock ~clear ~start ~binary_in =
    let binary_in = uextend binary_in ~width:binary_bits in
    let spec = Reg_spec.create ~clock ~clear () in
    (* Registers to latch the input binary value and count through it's bits. *)
    let binary = Always.Variable.reg spec ~width:binary_bits in
    let bit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 binary_bits) in
    (* Register to count through digits while dabbling. *)
    let digit_count = Always.Variable.reg spec ~width:(Int.ceil_log2 num_digits) in
    (* One digit count to use as a register write enable for the bcd digits *)
    let digit_count_one_hot = binary_to_onehot digit_count.value in
    (* Registers for the BCD digit *)
    let bcd = Array.init num_digits ~f:(fun _ -> Always.Variable.reg spec ~width:4) in
    (* Dabbling logic - look up the current bcd digit and perform dabble operation if
       greater than 4 *)
    let bcd_dabbled =
      let digit =
        mux digit_count.value (List.map (Array.to_list bcd) ~f:(fun bcd -> bcd.value))
      in
      mux2 (digit >:. 4) (digit +:. 3) digit
    in
    (* Statemachine *)
    let sm = Always.State_machine.create (module State) spec in
    Always.(
      compile
        [ sm.switch
            [ ( Start
              , [ (* Wait for start *)
                  bit_count <--. 0
                ; binary <-- binary_in
                ; when_
                    start
                    [ proc (List.init num_digits ~f:(fun digit -> bcd.(digit) <--. 0))
                    ; sm.set_next Double
                    ]
                ] )
            ; ( Double
              , [ (* Shift in the next binary bit through all the BCD registers. *)
                  binary <-- sll binary.value ~by:1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       bcd.(digit)
                       <-- lsbs bcd.(digit).value
                           @:
                           if digit = 0
                           then msb binary.value
                           else msb bcd.(digit - 1).value))
                ; digit_count <--. 0
                ; bit_count <-- bit_count.value +:. 1
                ; (* Count through all the input binary bits *)
                  if_
                    (bit_count.value ==:. binary_bits - 1)
                    [ sm.set_next Start ]
                    [ sm.set_next Dabble ]
                ] )
            ; ( Dabble
              , [ (* Iterate through each digit and perform the dabble operation. *)
                  digit_count <-- digit_count.value +:. 1
                ; proc
                    (List.init num_digits ~f:(fun digit ->
                       when_ digit_count_one_hot.:(digit) [ bcd.(digit) <-- bcd_dabbled ]))
                ; when_ (digit_count.value ==:. num_digits - 1) [ sm.set_next Double ]
                ] )
            ]
        ]);
    sm.is Start, Array.map bcd ~f:(fun bcd -> bcd.value)
  ;;
end

module BCD = Make_bcd (struct
    let num_digits = 5
  end)

module Tracer = struct
  module State = struct
    type t =
      | Not_started
      | Stepping
      | Emitting
      | Extra_step
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; ready : 'a
      ; cpu : 'a Cpu.O.t
      ; rx_start : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { cpu_enable : 'a
      ; tx : 'a [@bits 8]
      ; tx_start : 'a
      }
    [@@deriving hardcaml]
  end

  let max_cycle = 26554
  let trace_format = ".... A:.. X:.. Y:.. P:.. S:.. CYC:.....\r\n"

  let create scope (i : _ I.t) : _ O.t =
    let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let open Always in
    let%hw.State_machine state = State_machine.create (module State) reg_spec in
    let%hw_var tx_start = Variable.wire ~default:gnd () in
    let%hw_var sent = Variable.reg ~width:1 reg_spec in
    let%hw_var cpu_enable = Variable.wire ~default:gnd () in
    let%hw_var cycles' =
      Always.Variable.reg ~width:(num_bits_to_represent (max_cycle * 2)) reg_spec
    in
    let cycles = drop_bottom cycles'.value ~width:1 in
    let nbl s n = select s ~high:((4 * (n + 1)) - 1) ~low:(4 * n) in
    let%hw_var start_bcd = Variable.wire ~default:gnd () in
    let _, bcd =
      BCD.create ~clock:i.clock ~clear:i.clear ~start:start_bcd.value ~binary_in:cycles
    in
    let values =
      [| nbl i.cpu.pc 3
       ; nbl i.cpu.pc 2
       ; nbl i.cpu.pc 1
       ; nbl i.cpu.pc 0
       ; nbl i.cpu.a 1
       ; nbl i.cpu.a 0
       ; nbl i.cpu.x 1
       ; nbl i.cpu.x 0
       ; nbl i.cpu.y 1
       ; nbl i.cpu.y 0
       ; nbl i.cpu.p 1
       ; nbl i.cpu.p 0
       ; nbl i.cpu.s 1
       ; nbl i.cpu.s 0
       ; bcd.(4)
       ; bcd.(3)
       ; bcd.(2)
       ; bcd.(1)
       ; bcd.(0)
      |]
    in
    let num_spaces = String.count trace_format ~f:([%equal: char] '.') in
    [%test_eq: int] num_spaces (Array.length values);
    let%hw_var step =
      Always.Variable.reg
        ~width:(num_bits_to_represent (String.length trace_format))
        reg_spec
    in
    let param =
      mux
        step.value
        (String.to_list trace_format
         |> List.folding_map ~init:0 ~f:(fun idx c ->
           match c with
           | '.' -> idx + 1, values.(idx)
           | _ -> idx, of_unsigned_int 0xA ~width:4))
    in
    let param_byte =
      mux param (String.to_list "0123456789ABCDEF" |> List.map ~f:of_char)
    in
    let tx =
      mux
        step.value
        (String.to_list trace_format
         |> List.map ~f:(function
           | '.' -> param_byte
           | c -> of_char c))
    in
    compile
      [ state.switch
          [ Not_started, [ when_ i.rx_start [ state.set_next Stepping ] ]
          ; ( Stepping
            , [ if_
                  i.cpu.fetching
                  [ when_
                      (i.ready &: (cycles <:. max_cycle))
                      [ state.set_next Emitting; start_bcd <-- vdd; step <--. 0 ]
                  ]
                  [ cpu_enable <-- vdd ]
              ] )
          ; ( Emitting
            , [ if_
                  sent.value
                  [ when_
                      i.ready
                      [ sent <-- gnd
                      ; if_
                          (step.value ==:. String.length trace_format - 1)
                          [ state.set_next Extra_step; cpu_enable <-- vdd ]
                          [ incr step ]
                      ]
                  ]
                  [ tx_start <-- vdd; sent <-- vdd ]
              ] )
          ; Extra_step, [ cpu_enable <-- vdd; state.set_next Stepping ]
          ]
      ; when_ cpu_enable.value [ incr cycles' ]
      ];
    { cpu_enable = cpu_enable.value; tx; tx_start = tx_start.value }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"tracer" create input
  ;;
end

module Simulator = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; tx_ready : 'a
      ; rx_start : 'a
      }
  end

  module O = struct
    type 'a t =
      { tx : 'a [@bits 8]
      ; tx_start : 'a
      }
  end

  let flipper ~clock ~clear ~enable =
    reg_fb (Reg_spec.create ~clock ~clear ()) ~enable ~width:1 ~f:(fun x -> ~:x)
  ;;

  let create scope (i : _ I.t) : _ O.t =
    let%hw cpu_enable = wire 1 in
    let cpu_tick = flipper ~clock:i.clock ~clear:i.clear ~enable:cpu_enable in
    let%hw addr = wire 16 in
    let%hw write = wire 1 in
    let%hw write_data = wire 8 in
    let memory =
      Memory.hierarchical
        scope
        { clock = i.clock
        ; enable = cpu_enable &: ~:cpu_tick
        ; addr
        ; write
        ; data_in = write_data
        }
    in
    let cpu =
      Cpu.hierarchical
        scope
        { clock = i.clock
        ; clear = i.clear
        ; enable = cpu_enable &: cpu_tick
        ; irq = gnd
        ; nmi = gnd
        ; data = memory.data
        }
    in
    assign addr cpu.mem.addr;
    assign write cpu.mem.write;
    assign write_data cpu.mem.data;
    let tracer =
      Tracer.hierarchical
        scope
        { clock = i.clock
        ; clear = i.clear
        ; ready = i.tx_ready
        ; cpu
        ; rx_start = i.rx_start
        }
    in
    assign cpu_enable tracer.cpu_enable;
    { tx = tracer.tx; tx_start = tracer.tx_start }
  ;;
end

let create_uart board scope ~clock ~clear ~tx ~tx_start =
  let uart = Nexys.Uart.create board in
  let config : _ Uart.Config.t =
    { data_bits = Uart_types.Data_bits.Enum.Of_signal.of_enum Eight
    ; parity = Uart_types.Parity.Enum.Of_signal.of_enum Even
    ; stop_bits = Uart_types.Stop_bits.Enum.Of_signal.of_enum One
    ; clocks_per_bit =
        of_unsigned_int
          ~width:Uart.Config.port_widths.clocks_per_bit
          (master_clock_hz / 115_200)
    }
  in
  let uart_rx =
    Uart.Rx.create
      ~align_rxdata_to_lsb:true
      scope
      { clocking = { clock; clear }; enable = vdd; config; rxd = uart.rxd }
  in
  let uart_tx =
    Uart.Tx.create
      scope
      { clocking = { clock; clear }
      ; config
      ; data_in = gnd @: tx
      ; data_in_valid = tx_start
      }
  in
  Nexys.Uart.complete board { rts = gnd; txd = uart_tx.txd };
  uart_tx.data_in_ready, uart_rx.data_out_valid
;;

module Clk_wiz = struct
  module I = struct
    type 'a t =
      { clk_in1 : 'a
      ; reset : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { clk_out1 : 'a
      ; locked : 'a
      }
    [@@deriving hardcaml]
  end

  let create i =
    let module Inst = Hardcaml.Instantiation.With_interface (I) (O) in
    Inst.create ~name:"clk_wiz" i
  ;;
end

let get_master_clock board =
  let clocking = Nexys.Clock_and_reset.create board in
  let o = Clk_wiz.create { clk_in1 = clocking.clock_100; reset = ~:(clocking.reset_n) } in
  ~clock:o.clk_out1, ~clear:~:(o.locked)
;;

let create () =
  let board = Board.create () in
  let scope = Board.scope board in
  let ~clock, ~clear = get_master_clock board in
  let tx_ready = wire 1 in
  let rx_start = wire 1 in
  let simulator = Simulator.create scope { clock; clear; tx_ready; rx_start } in
  let tx_ready', rx_start' =
    create_uart board scope ~clock ~clear ~tx:simulator.tx ~tx_start:simulator.tx_start
  in
  assign tx_ready tx_ready';
  assign rx_start rx_start';
  board
;;
