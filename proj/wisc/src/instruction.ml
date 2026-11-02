open! Core
open Ints

module Op = struct
  type arith =
    [ `ADD
    | `SUB
    | `MUL
    | `SLU
    | `SLT
    | `AND
    | `BOR
    | `XOR
    | `SHR
    | `SHL
    | `ASR
    ]
  [@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

  type branch =
    [ `BGZ
    | `BEZ
    ]
  [@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

  type jump =
    [ `JLR
    | `JAL
    ]
  [@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]

  type t =
    [ arith
    | `LDA
    | `STO
    | branch
    | jump
    ]
  [@@deriving compare, equal, enumerate, string, quickcheck ~generator, sexp]
end

(* The branch conditions share an opcode, and are told apart by a bit of the instruction. *)
module Opcode = struct
  type t =
    [ Op.arith
    | `LDA
    | `STO
    | `BRANCH
    | Op.jump
    ]
  [@@deriving compare, equal, enumerate, sexp]

  let of_op : Op.t -> t = function
    | #Op.branch -> `BRANCH
    | (#Op.arith | `LDA | `STO | #Op.jump) as op -> op
  ;;

  let to_int (t : [< t ]) =
    match t with
    | `ADD -> 0x0
    | `SUB -> 0x1
    | `MUL -> 0x2
    | `SLU -> 0x3
    | `SLT -> 0x4
    | `AND -> 0x5
    | `BOR -> 0x6
    | `XOR -> 0x7
    | `SHR -> 0x8
    | `SHL -> 0x9
    | `ASR -> 0xa
    | `LDA -> 0xb
    | `STO -> 0xc
    | `BRANCH -> 0xd
    | `JLR -> 0xe
    | `JAL -> 0xf
  ;;

  let of_int n : t option =
    match n with
    | 0x0 -> Some `ADD
    | 0x1 -> Some `SUB
    | 0x2 -> Some `MUL
    | 0x3 -> Some `SLU
    | 0x4 -> Some `SLT
    | 0x5 -> Some `AND
    | 0x6 -> Some `BOR
    | 0x7 -> Some `XOR
    | 0x8 -> Some `SHR
    | 0x9 -> Some `SHL
    | 0xa -> Some `ASR
    | 0xb -> Some `LDA
    | 0xc -> Some `STO
    | 0xd -> Some `BRANCH
    | 0xe -> Some `JLR
    | 0xf -> Some `JAL
    | _ -> None
  ;;
end

let branch_condition_bit : Op.branch -> int = function
  | `BEZ -> 0
  | `BGZ -> 1
;;

type t =
  | Arith of
      { op : Op.arith
      ; rd : Register.t
      ; rs1 : Register.t
      ; rs2 : Register.t
      }
  | LDA of
      { rd : Register.t
      ; addr : Register.t
      ; offset : U4.t
      }
  | STO of
      { addr : Register.t
      ; rs : Register.t
      ; offset : U4.t
      }
  | Branch of
      { op : Op.branch
      ; rs : Register.t
      ; offset : I7.t
      }
  | Jump of
      { op : Op.jump
      ; rd : Register.t
      ; rs1 : Register.t
      ; rs2 : Register.t
      }
[@@deriving compare, equal, quickcheck ~generator, sexp]

let join_parts (type n) (parts : (int, n) Fixed_list.t) ~(widths : (int, n) Fixed_list.t) =
  Fixed_list.fold2 widths parts ~init:0 ~f:(fun acc width part ->
    let size = 1 lsl width in
    let part = if part < 0 then size + part else part in
    (acc lsl width) lor (part land (size - 1)))
;;

let to_int = function
  | Arith { op; rd; rs1; rs2 } ->
    join_parts
      ~widths:[ 4; 4; 4; 4 ]
      [ Opcode.to_int op; Register.to_int rd; Register.to_int rs1; Register.to_int rs2 ]
  | Jump { op; rd; rs1; rs2 } ->
    join_parts
      ~widths:[ 4; 4; 4; 4 ]
      [ Opcode.to_int op; Register.to_int rd; Register.to_int rs1; Register.to_int rs2 ]
  | LDA { rd; addr; offset } ->
    join_parts
      ~widths:[ 4; 4; 4; 4 ]
      [ Opcode.to_int `LDA; Register.to_int rd; Register.to_int addr; U4.to_int offset ]
  | STO { addr; rs; offset } ->
    join_parts
      ~widths:[ 4; 4; 4; 4 ]
      [ Opcode.to_int `STO; U4.to_int offset; Register.to_int addr; Register.to_int rs ]
  | Branch { op; rs; offset } ->
    join_parts
      ~widths:[ 4; 1; 7; 4 ]
      [ Opcode.to_int `BRANCH
      ; branch_condition_bit op
      ; I7.to_int offset
      ; Register.to_int rs
      ]
;;
