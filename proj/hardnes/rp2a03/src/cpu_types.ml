open! Core
open! Hardcaml

module type With_enums = functor (Cases : T) -> sig
  include Enum.S_enums with module Cases := Cases
end

module Reg = struct
  type t =
    [ `A
    | `X
    | `Y
    | `S
    ]
  [@@deriving string ~case_insensitive, sexp, compare ~localize, ~enumerate]

  module AXY = struct
    type t =
      [ `A
      | `X
      | `Y
      ]
    [@@deriving string ~case_insensitive, sexp, compare ~localize, ~enumerate]
  end

  module XY = struct
    type t =
      [ `X
      | `Y
      ]
    [@@deriving string ~case_insensitive, sexp, compare ~localize, ~enumerate]
  end
end

module Flag = struct
  type t =
    | N
    | V
    | D
    | I
    | Z
    | C
  [@@deriving string ~case_insensitive, compare ~localize, ~enumerate]

  include functor Sexpable.Of_stringable
end

module Op = struct
  module Read = struct
    type t =
      | NOP
      | ADC
      | AND
      | BIT
      | CP of Reg.AXY.t [@nested "CP"]
      | EOR
      | LAX
      | LD of Reg.AXY.t [@nested "LD"]
      | ORA
      | SBC
      | ANC
      | ALR
      | ARR
      | AXS
    [@@deriving string ~case_insensitive, compare ~localize, ~enumerate]

    include functor Sexpable.Of_stringable
  end

  module Write = struct
    type t =
      | ST of Reg.AXY.t [@nested "ST"]
      | SAX
    [@@deriving string ~case_insensitive, compare ~localize, ~enumerate]

    include functor Sexpable.Of_stringable
    include functor Enum.Make_enums
  end

  module Read_write = struct
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
    [@@deriving string ~case_insensitive, compare ~localize, ~enumerate]

    include functor Sexpable.Of_stringable
    include functor Enum.Make_enums
  end

  type t =
    | Read of Read.t [@nested ""]
    | Write of Write.t [@nested ""]
    | Read_write of Read_write.t [@nested ""]
    | SH of Reg.AXY.t [@nested "SH"]
  [@@deriving string ~case_insensitive, compare ~localize, ~enumerate]

  include functor Sexpable.Of_stringable
  include functor Enum.Make_enums
end

module Decoded = struct
  type t =
    | Illegal
    | Immediate of Op.Read.t
    | Accumulator of Op.Read_write.t
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
    (* Branches *)
    | Branch of Flag.t * bool
    (* Special ops*)
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

  let op = function
    | Immediate op -> Some (Op.Read op)
    | Accumulator op -> Some (Op.Read_write op)
    | Indirect_X op
    | Indirect_Y op
    | Zero_page op
    | Zero_page_indexed (op, _)
    | Absolute op
    | Absolute_indexed (op, _) -> Some op
    | _ -> None
  ;;

  let of_char = function
    | '\x00' -> BRK
    | '\x20' -> JSR
    | '\x40' -> RTI
    | '\x60' -> RTS
    | '\x80' -> Immediate NOP
    | '\xA0' -> Immediate (LD `Y)
    | '\xC0' -> Immediate (CP `Y)
    | '\xE0' -> Immediate (CP `X)
    | '\x01' -> Indirect_X (Read ORA)
    | '\x21' -> Indirect_X (Read AND)
    | '\x41' -> Indirect_X (Read EOR)
    | '\x61' -> Indirect_X (Read ADC)
    | '\x81' -> Indirect_X (Write (ST `A))
    | '\xA1' -> Indirect_X (Read (LD `A))
    | '\xC1' -> Indirect_X (Read (CP `A))
    | '\xE1' -> Indirect_X (Read SBC)
    | '\x82' -> Immediate NOP
    | '\xA2' -> Immediate (LD `X)
    | '\xC2' -> Immediate NOP
    | '\xE2' -> Immediate NOP
    | '\x03' -> Indirect_X (Read_write SLO)
    | '\x23' -> Indirect_X (Read_write RLA)
    | '\x43' -> Indirect_X (Read_write SRE)
    | '\x63' -> Indirect_X (Read_write RRA)
    | '\x83' -> Indirect_X (Write SAX)
    | '\xA3' -> Indirect_X (Read LAX)
    | '\xC3' -> Indirect_X (Read_write DCP)
    | '\xE3' -> Indirect_X (Read_write ISC)
    | '\x04' -> Zero_page (Read NOP)
    | '\x24' -> Zero_page (Read BIT)
    | '\x44' -> Zero_page (Read NOP)
    | '\x64' -> Zero_page (Read NOP)
    | '\x84' -> Zero_page (Write (ST `Y))
    | '\xA4' -> Zero_page (Read (LD `Y))
    | '\xC4' -> Zero_page (Read (CP `Y))
    | '\xE4' -> Zero_page (Read (CP `X))
    | '\x05' -> Zero_page (Read ORA)
    | '\x25' -> Zero_page (Read AND)
    | '\x45' -> Zero_page (Read EOR)
    | '\x65' -> Zero_page (Read ADC)
    | '\x85' -> Zero_page (Write (ST `A))
    | '\xA5' -> Zero_page (Read (LD `A))
    | '\xC5' -> Zero_page (Read (CP `A))
    | '\xE5' -> Zero_page (Read SBC)
    | '\x06' -> Zero_page (Read_write ASL)
    | '\x26' -> Zero_page (Read_write ROL)
    | '\x46' -> Zero_page (Read_write LSR)
    | '\x66' -> Zero_page (Read_write ROR)
    | '\x86' -> Zero_page (Write (ST `X))
    | '\xA6' -> Zero_page (Read (LD `X))
    | '\xC6' -> Zero_page (Read_write DEC)
    | '\xE6' -> Zero_page (Read_write INC)
    | '\x07' -> Zero_page (Read_write SLO)
    | '\x27' -> Zero_page (Read_write RLA)
    | '\x47' -> Zero_page (Read_write SRE)
    | '\x67' -> Zero_page (Read_write RRA)
    | '\x87' -> Zero_page (Write SAX)
    | '\xA7' -> Zero_page (Read LAX)
    | '\xC7' -> Zero_page (Read_write DCP)
    | '\xE7' -> Zero_page (Read_write ISC)
    | '\x08' -> PHP
    | '\x28' -> PLP
    | '\x48' -> PHA
    | '\x68' -> PLA
    | '\x88' -> Dec `Y
    | '\xA8' -> Transfer (`A, `Y)
    | '\xC8' -> Inc `Y
    | '\xE8' -> Inc `X
    | '\x09' -> Immediate ORA
    | '\x29' -> Immediate AND
    | '\x49' -> Immediate EOR
    | '\x69' -> Immediate ADC
    | '\x89' -> Immediate NOP
    | '\xA9' -> Immediate (LD `A)
    | '\xC9' -> Immediate (CP `A)
    | '\xE9' -> Immediate SBC
    | '\x0A' -> Accumulator ASL
    | '\x2A' -> Accumulator ROL
    | '\x4A' -> Accumulator LSR
    | '\x6A' -> Accumulator ROR
    | '\x8A' -> Transfer (`X, `A)
    | '\xAA' -> Transfer (`A, `X)
    | '\xCA' -> Dec `X
    | '\xEA' -> Nop
    | '\x0B' -> Immediate ANC
    | '\x2B' -> Immediate ANC
    | '\x4B' -> Immediate ALR
    | '\x6B' -> Immediate ARR
    | '\xAB' -> Immediate LAX
    | '\xCB' -> Immediate AXS
    | '\xEB' -> Immediate SBC
    | '\x0C' -> Absolute (Read NOP)
    | '\x2C' -> Absolute (Read BIT)
    | '\x4C' -> JMP
    | '\x6C' -> JMPI
    | '\x8C' -> Absolute (Write (ST `Y))
    | '\xAC' -> Absolute (Read (LD `Y))
    | '\xCC' -> Absolute (Read (CP `Y))
    | '\xEC' -> Absolute (Read (CP `X))
    | '\x0D' -> Absolute (Read ORA)
    | '\x2D' -> Absolute (Read AND)
    | '\x4D' -> Absolute (Read EOR)
    | '\x6D' -> Absolute (Read ADC)
    | '\x8D' -> Absolute (Write (ST `A))
    | '\xAD' -> Absolute (Read (LD `A))
    | '\xCD' -> Absolute (Read (CP `A))
    | '\xED' -> Absolute (Read SBC)
    | '\x0E' -> Absolute (Read_write ASL)
    | '\x2E' -> Absolute (Read_write ROL)
    | '\x4E' -> Absolute (Read_write LSR)
    | '\x6E' -> Absolute (Read_write ROR)
    | '\x8E' -> Absolute (Write (ST `X))
    | '\xAE' -> Absolute (Read (LD `X))
    | '\xCE' -> Absolute (Read_write DEC)
    | '\xEE' -> Absolute (Read_write INC)
    | '\x0F' -> Absolute (Read_write SLO)
    | '\x2F' -> Absolute (Read_write RLA)
    | '\x4F' -> Absolute (Read_write SRE)
    | '\x6F' -> Absolute (Read_write RRA)
    | '\x8F' -> Absolute (Write SAX)
    | '\xAF' -> Absolute (Read LAX)
    | '\xCF' -> Absolute (Read_write DCP)
    | '\xEF' -> Absolute (Read_write ISC)
    | '\x10' -> Branch (N, false)
    | '\x30' -> Branch (N, true)
    | '\x50' -> Branch (V, false)
    | '\x70' -> Branch (V, true)
    | '\x90' -> Branch (C, false)
    | '\xB0' -> Branch (C, true)
    | '\xD0' -> Branch (Z, false)
    | '\xF0' -> Branch (Z, true)
    | '\x11' -> Indirect_Y (Read ORA)
    | '\x31' -> Indirect_Y (Read AND)
    | '\x51' -> Indirect_Y (Read EOR)
    | '\x71' -> Indirect_Y (Read ADC)
    | '\x91' -> Indirect_Y (Write (ST `A))
    | '\xB1' -> Indirect_Y (Read (LD `A))
    | '\xD1' -> Indirect_Y (Read (CP `A))
    | '\xF1' -> Indirect_Y (Read SBC)
    | '\x13' -> Indirect_Y (Read_write SLO)
    | '\x33' -> Indirect_Y (Read_write RLA)
    | '\x53' -> Indirect_Y (Read_write SRE)
    | '\x73' -> Indirect_Y (Read_write RRA)
    | '\x93' -> Indirect_Y (SH `X)
    | '\xB3' -> Indirect_Y (Read LAX)
    | '\xD3' -> Indirect_Y (Read_write DCP)
    | '\xF3' -> Indirect_Y (Read_write ISC)
    | '\x14' -> Zero_page_indexed (Read NOP, `X)
    | '\x34' -> Zero_page_indexed (Read NOP, `X)
    | '\x54' -> Zero_page_indexed (Read NOP, `X)
    | '\x74' -> Zero_page_indexed (Read NOP, `X)
    | '\x94' -> Zero_page_indexed (Write (ST `Y), `X)
    | '\xB4' -> Zero_page_indexed (Read (LD `Y), `X)
    | '\xD4' -> Zero_page_indexed (Read NOP, `X)
    | '\xF4' -> Zero_page_indexed (Read NOP, `X)
    | '\x15' -> Zero_page_indexed (Read ORA, `X)
    | '\x35' -> Zero_page_indexed (Read AND, `X)
    | '\x55' -> Zero_page_indexed (Read EOR, `X)
    | '\x75' -> Zero_page_indexed (Read ADC, `X)
    | '\x95' -> Zero_page_indexed (Write (ST `A), `X)
    | '\xB5' -> Zero_page_indexed (Read (LD `A), `X)
    | '\xD5' -> Zero_page_indexed (Read (CP `A), `X)
    | '\xF5' -> Zero_page_indexed (Read SBC, `X)
    | '\x16' -> Zero_page_indexed (Read_write ASL, `X)
    | '\x36' -> Zero_page_indexed (Read_write ROL, `X)
    | '\x56' -> Zero_page_indexed (Read_write LSR, `X)
    | '\x76' -> Zero_page_indexed (Read_write ROR, `X)
    | '\x96' -> Zero_page_indexed (Write (ST `X), `Y)
    | '\xB6' -> Zero_page_indexed (Read (LD `X), `Y)
    | '\xD6' -> Zero_page_indexed (Read_write DEC, `X)
    | '\xF6' -> Zero_page_indexed (Read_write INC, `X)
    | '\x17' -> Zero_page_indexed (Read_write SLO, `X)
    | '\x37' -> Zero_page_indexed (Read_write RLA, `X)
    | '\x57' -> Zero_page_indexed (Read_write SRE, `X)
    | '\x77' -> Zero_page_indexed (Read_write RRA, `X)
    | '\x97' -> Zero_page_indexed (Write SAX, `Y)
    | '\xB7' -> Zero_page_indexed (Read LAX, `Y)
    | '\xD7' -> Zero_page_indexed (Read_write DCP, `X)
    | '\xF7' -> Zero_page_indexed (Read_write ISC, `X)
    | '\x18' -> Clear C
    | '\x38' -> Set C
    | '\x58' -> Clear I
    | '\x78' -> Set I
    | '\x98' -> Transfer (`Y, `A)
    | '\xB8' -> Clear V
    | '\xD8' -> Clear D
    | '\xF8' -> Set D
    | '\x19' -> Absolute_indexed (Read ORA, `Y)
    | '\x39' -> Absolute_indexed (Read AND, `Y)
    | '\x59' -> Absolute_indexed (Read EOR, `Y)
    | '\x79' -> Absolute_indexed (Read ADC, `Y)
    | '\x99' -> Absolute_indexed (Write (ST `A), `Y)
    | '\xB9' -> Absolute_indexed (Read (LD `A), `Y)
    | '\xD9' -> Absolute_indexed (Read (CP `A), `Y)
    | '\xF9' -> Absolute_indexed (Read SBC, `Y)
    | '\x1A' -> Nop
    | '\x3A' -> Nop
    | '\x5A' -> Nop
    | '\x7A' -> Nop
    | '\x9A' -> Transfer (`X, `S)
    | '\xBA' -> Transfer (`S, `X)
    | '\xDA' -> Nop
    | '\xFA' -> Nop
    | '\x1B' -> Absolute_indexed (Read_write SLO, `Y)
    | '\x3B' -> Absolute_indexed (Read_write RLA, `Y)
    | '\x5B' -> Absolute_indexed (Read_write SRE, `Y)
    | '\x7B' -> Absolute_indexed (Read_write RRA, `Y)
    | '\xDB' -> Absolute_indexed (Read_write DCP, `Y)
    | '\xFB' -> Absolute_indexed (Read_write ISC, `Y)
    | '\x1C' -> Absolute_indexed (Read NOP, `X)
    | '\x3C' -> Absolute_indexed (Read NOP, `X)
    | '\x5C' -> Absolute_indexed (Read NOP, `X)
    | '\x7C' -> Absolute_indexed (Read NOP, `X)
    | '\x9C' -> Absolute_indexed (SH `Y, `X)
    | '\xBC' -> Absolute_indexed (Read (LD `Y), `X)
    | '\xDC' -> Absolute_indexed (Read NOP, `X)
    | '\xFC' -> Absolute_indexed (Read NOP, `X)
    | '\x1D' -> Absolute_indexed (Read ORA, `X)
    | '\x3D' -> Absolute_indexed (Read AND, `X)
    | '\x5D' -> Absolute_indexed (Read EOR, `X)
    | '\x7D' -> Absolute_indexed (Read ADC, `X)
    | '\x9D' -> Absolute_indexed (Write (ST `A), `X)
    | '\xBD' -> Absolute_indexed (Read (LD `A), `X)
    | '\xDD' -> Absolute_indexed (Read (CP `A), `X)
    | '\xFD' -> Absolute_indexed (Read SBC, `X)
    | '\x1E' -> Absolute_indexed (Read_write ASL, `X)
    | '\x3E' -> Absolute_indexed (Read_write ROL, `X)
    | '\x5E' -> Absolute_indexed (Read_write LSR, `X)
    | '\x7E' -> Absolute_indexed (Read_write ROR, `X)
    | '\x9E' -> Absolute_indexed (SH `X, `Y)
    | '\xBE' -> Absolute_indexed (Read (LD `X), `Y)
    | '\xDE' -> Absolute_indexed (Read_write DEC, `X)
    | '\xFE' -> Absolute_indexed (Read_write INC, `X)
    | '\x1F' -> Absolute_indexed (Read_write SLO, `X)
    | '\x3F' -> Absolute_indexed (Read_write RLA, `X)
    | '\x5F' -> Absolute_indexed (Read_write SRE, `X)
    | '\x7F' -> Absolute_indexed (Read_write RRA, `X)
    | '\x9F' -> Absolute_indexed (SH `A, `Y)
    | '\xBF' -> Absolute_indexed (Read LAX, `Y)
    | '\xDF' -> Absolute_indexed (Read_write DCP, `X)
    | '\xFF' -> Absolute_indexed (Read_write ISC, `X)
    | _ -> Illegal
  ;;

  let cases f =
    List.map Char.all ~f:(fun c ->
      Signal.of_unsigned_int (Char.to_int c) ~width:8, f (of_char c))
  ;;

  let switch f =
    List.filter_map Char.all ~f:(fun c ->
      match f (of_char c) with
      | [] -> None
      | l -> Some (Signal.of_unsigned_int (Char.to_int c) ~width:8, l))
  ;;
end
