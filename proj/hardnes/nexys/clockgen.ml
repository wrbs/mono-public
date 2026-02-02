open! Core
open! Hardcaml
open! Signal

let ibuf in_ =
  let inst =
    Instantiation.create ~name:"IBUF" ~inputs:[ "I", in_ ] ~outputs:[ "O", 1 ] ()
  in
  Instantiation.output inst "O"
;;

let bufg in_ =
  let inst =
    Instantiation.create ~name:"BUFG" ~inputs:[ "I", in_ ] ~outputs:[ "O", 1 ] ()
  in
  Instantiation.output inst "O"
;;

let mmce2_base
  ~clkin1_period
  ~divclk_divide
  ~clkfbout_mult_f
  ~clkout0_divide_f
  ~clkin1
  ~clkfbin
  ~reset
  =
  let inst =
    Instantiation.create
      ~name:"MMCME2_BASE"
      ~parameters:
        [ Parameter.create ~name:"BANDWIDTH" ~value:(String "OPTIMIZED")
        ; Parameter.create ~name:"CLKIN1_PERIOD" ~value:(Real clkin1_period)
        ; Parameter.create ~name:"DIVCLK_DIVIDE" ~value:(Int divclk_divide)
        ; Parameter.create ~name:"CLKFBOUT_MULT_F" ~value:(Real clkfbout_mult_f)
        ; Parameter.create ~name:"CLKOUT0_DIVIDE_F" ~value:(Real clkout0_divide_f)
        ]
      ~inputs:[ "CLKIN1", clkin1; "CLKFBIN", clkfbin; "PWRDWN", gnd; "RST", reset ]
      ~outputs:
        [ "CLKOUT0", 1
        ; "CLKOUT0B", 1
        ; "CLKOUT1", 1
        ; "CLKOUT1B", 1
        ; "CLKOUT2", 1
        ; "CLKOUT2B", 1
        ; "CLKOUT3", 1
        ; "CLKOUT3B", 1
        ; "CLKOUT4", 1
        ; "CLKOUT5", 1
        ; "CLKOUT6", 1
        ; "CLKFBOUT", 1
        ; "CLKFBOUTB", 1
        ; "LOCKED", 1
        ]
      ()
  in
  ( ~clkout0:(Instantiation.output inst "CLKOUT0")
  , ~clkfbout:(Instantiation.output inst "CLKFBOUT")
  , ~locked:(Instantiation.output inst "LOCKED") )
;;

module Nes_master_clock = struct
  module I = struct
    type 'a t =
      { clock_100 : 'a
      ; reset : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { clock : 'a
      ; locked : 'a
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let%hw clk_in_buf = ibuf i.clock_100 in
    let%hw clkfb_buf = wire 1 in
    let%hw ~clkout0, ~clkfbout, ~locked =
      mmce2_base
        ~clkin1_period:10.
        ~divclk_divide:5
        ~clkfbout_mult_f:47.25
        ~clkout0_divide_f:44.
        ~clkin1:clk_in_buf
        ~clkfbin:clkfb_buf
        ~reset:i.reset
    in
    assign clkfb_buf (bufg clkfbout);
    let%hw clock = bufg clkout0 in
    { clock; locked }
  ;;

  let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
    let module H = Hierarchy.In_scope (I) (O) in
    H.hierarchical ~scope ~name:"nes_master_clock" create input
  ;;
end
