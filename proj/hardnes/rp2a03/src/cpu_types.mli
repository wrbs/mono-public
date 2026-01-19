open! Core
open! Hardcaml

module type With_enums = functor (Cases : T) -> Enum.S_enums with module Cases := Cases

module Reg : sig
  type t =
    [ `A
    | `X
    | `Y
    | `S
    ]
  [@@deriving string, sexp, compare ~localize, ~enumerate]

  module AXY : sig
    type t =
      [ `A
      | `X
      | `Y
      ]
    [@@deriving string, sexp, compare ~localize, ~enumerate]
  end

  module XY : sig
    type t =
      [ `X
      | `Y
      ]
    [@@deriving string, sexp, compare ~localize, ~enumerate]
  end
end

module Flag : sig
  type t =
    | N
    | V
    | D
    | I
    | Z
    | C
  [@@deriving string, compare ~localize, ~enumerate]
end

module Op : sig
  module Read : sig
    type t =
      | NOP
      | ADC
      | AND
      | BIT
      | CP of Reg.AXY.t
      | EOR
      | LAX
      | LD of Reg.AXY.t
      | ORA
      | SBC
      | ANC
      | ALR
      | ARR
      | AXS
    [@@deriving string, sexp, compare ~localize, ~enumerate]
  end

  module Write : sig
    type t =
      | ST of Reg.AXY.t
      | SAX
    [@@deriving string, sexp, compare ~localize, ~enumerate]
  end

  module Read_write : sig
    type t =
      | ASL
      | DEC
      | INC
      | LSR
      | ROL
      | ROR
      | DCP
      | ISC
      | SLO
      | RLA
      | SRE
      | RRA
    [@@deriving string, sexp, compare ~localize, ~enumerate]
  end

  type t =
    | Read of Read.t
    | Write of Write.t
    | Read_write of Read_write.t
    | SH of Reg.AXY.t
  [@@deriving string, sexp, compare ~localize, ~enumerate]

  include functor With_enums
end

module Decoded : sig
  type t =
    | Illegal
    (* Special *)
    | Immediate of Op.Read.t
    | Accumulator of Op.Read_write.t
    (* Standard memory ops *)
    | Indirect_X of Op.t
    | Indirect_Y of Op.t
    | Zero_page of Op.t
    | Zero_page_indexed of Op.t * Reg.t
    | Absolute of Op.t
    | Absolute_indexed of Op.t * Reg.t
    (* Implied *)
    | Nop
    | Inc of Reg.t
    | Dec of Reg.t
    | Transfer of Reg.t * Reg.t
    | Set of Flag.t
    | Clear of Flag.t
    (* Others *)
    | Branch of Flag.t * bool
    | BRK
    | JSR
    | RTI
    | RTS
    | PHP
    | PHA
    | JMP
    | JMPI
    | PLP
    | PLA
  [@@deriving sexp_of]

  val op : t -> Op.t option
  val of_char : char -> t
  val cases : (t -> 'a) -> (Signal.t * 'a) list
  val switch : (t -> 'a list) -> (Signal.t * 'a list) list
end
