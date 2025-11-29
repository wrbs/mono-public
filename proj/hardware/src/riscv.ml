open! Core
open! Hardcaml

module Register = struct
  type t =
    | X0
    | X1
    | X2
    | X3
    | X4
    | X5
    | X6
    | X7
    | X8
    | X9
    | X10
    | X11
    | X12
    | X13
    | X14
    | X15
    | X16
    | X17
    | X18
    | X19
    | X20
    | X21
    | X22
    | X23
    | X24
    | X25
    | X26
    | X27
    | X28
    | X29
    | X30
    | X31
  [@@deriving string ~capitalize:"lowercase", compare ~localize, enumerate]

  include functor Sexpable.Of_stringable

  let width = 5

  let to_int = function
    | X0 -> 0
    | X1 -> 1
    | X2 -> 2
    | X3 -> 3
    | X4 -> 4
    | X5 -> 5
    | X6 -> 6
    | X7 -> 7
    | X8 -> 8
    | X9 -> 9
    | X10 -> 10
    | X11 -> 11
    | X12 -> 12
    | X13 -> 13
    | X14 -> 14
    | X15 -> 15
    | X16 -> 16
    | X17 -> 17
    | X18 -> 18
    | X19 -> 19
    | X20 -> 20
    | X21 -> 21
    | X22 -> 22
    | X23 -> 23
    | X24 -> 24
    | X25 -> 25
    | X26 -> 26
    | X27 -> 27
    | X28 -> 28
    | X29 -> 29
    | X30 -> 30
    | X31 -> 31
  ;;

  include functor Hardcaml_encoding.Make
end

module Opcode = struct
  type t =
    | Op
    | Op_imm
    | Lui
    | Auipc
    | Jal
    | Jalr
    | Load
    | Branch
    | Store
    | Misc_mem
    | System
  [@@deriving sexp, compare ~localize, enumerate]

  let width = 7

  let to_int = function
    | Load -> 0b00_000_11
    | Op_imm -> 0b00_100_11
    | Misc_mem -> 0b00_011_11
    | Auipc -> 0b00_101_11
    | Store -> 0b01_000_11
    | Op -> 0b01_100_11
    | Lui -> 0b01_101_11
    | Branch -> 0b11_000_11
    | Jalr -> 0b11_001_11
    | Jal -> 0b11_011_11
    | System -> 0b11_100_11
  ;;

  include functor Hardcaml_encoding.Make
end

module Instr_decode = struct
  type 'a t =
    { opcode : 'a Opcode.Enc.t
    ; rd : 'a Register.Enc.t
    ; funct3 : 'a [@bits 3]
    ; rs1 : 'a Register.Enc.t
    ; rs2 : 'a Register.Enc.t
    ; funct7 : 'a [@bits 7]
    ; imm_i : 'a [@bits 32]
    ; imm_s : 'a [@bits 32]
    ; imm_b : 'a [@bits 32]
    ; imm_u : 'a [@bits 32]
    ; imm_j : 'a [@bits 32]
    }
  [@@deriving hardcaml]

  let create' (type a) (module C : Comb.S with type t = a) inst : a t =
    let open C in
    let opcode = Opcode.Enc.of_raw (module C) inst.:[6, 0] in
    let rd = Register.Enc.of_raw (module C) inst.:[11, 7] in
    let funct3 = inst.:[14, 12] in
    let rs1 = Register.Enc.of_raw (module C) inst.:[19, 15] in
    let rs2 = Register.Enc.of_raw (module C) inst.:[24, 20] in
    let funct7 = inst.:[31, 25] in
    let imm parts = Signed.resize (concat_msb parts) 32 in
    let imm_i = imm [ inst.:[31, 25]; inst.:[24, 20] ] in
    let imm_s = imm [ inst.:[31, 25]; inst.:[11, 7] ] in
    let imm_b = imm [ inst.:(31); inst.:(7); inst.:[30, 25]; inst.:[11, 8]; gnd ] in
    let imm_u = imm [ inst.:[31, 12]; zero 12 ] in
    let imm_j =
      imm [ inst.:(31); inst.:[19, 12]; inst.:(20); inst.:[30, 25]; inst.:[24, 21]; gnd ]
    in
    { opcode; rd; funct3; rs1; rs2; funct7; imm_i; imm_s; imm_b; imm_u; imm_j }
  ;;

  let create = create' (module Signal)
  let of_bits = create' (module Bits)
end

module Int_registers = struct
  type 'a t =
    { x0 : 'a [@bits 32]
    ; x1 : 'a [@bits 32]
    ; x2 : 'a [@bits 32]
    ; x3 : 'a [@bits 32]
    ; x4 : 'a [@bits 32]
    ; x5 : 'a [@bits 32]
    ; x6 : 'a [@bits 32]
    ; x7 : 'a [@bits 32]
    ; x8 : 'a [@bits 32]
    ; x9 : 'a [@bits 32]
    ; x10 : 'a [@bits 32]
    ; x11 : 'a [@bits 32]
    ; x12 : 'a [@bits 32]
    ; x13 : 'a [@bits 32]
    ; x14 : 'a [@bits 32]
    ; x15 : 'a [@bits 32]
    ; x16 : 'a [@bits 32]
    ; x17 : 'a [@bits 32]
    ; x18 : 'a [@bits 32]
    ; x19 : 'a [@bits 32]
    ; x20 : 'a [@bits 32]
    ; x21 : 'a [@bits 32]
    ; x22 : 'a [@bits 32]
    ; x23 : 'a [@bits 32]
    ; x24 : 'a [@bits 32]
    ; x25 : 'a [@bits 32]
    ; x26 : 'a [@bits 32]
    ; x27 : 'a [@bits 32]
    ; x28 : 'a [@bits 32]
    ; x29 : 'a [@bits 32]
    ; x30 : 'a [@bits 32]
    ; x31 : 'a [@bits 32]
    }
end
