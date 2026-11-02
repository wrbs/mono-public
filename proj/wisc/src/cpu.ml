open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clocking : 'a Clocking.t
    ; enable : 'a
    ; data_in : 'a [@bits 16]
    ; init_pc : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module IO = struct
  type 'a t =
    { addr : 'a [@bits 16]
    ; data : 'a [@bits 16]
    ; enable : 'a
    ; write : 'a
    }
  [@@deriving hardcaml]
end

module Registers = struct
  type 'a t =
    { r1 : 'a [@bits 16]
    ; r2 : 'a [@bits 16]
    ; r3 : 'a [@bits 16]
    ; r4 : 'a [@bits 16]
    ; r5 : 'a [@bits 16]
    ; r6 : 'a [@bits 16]
    ; r7 : 'a [@bits 16]
    ; r8 : 'a [@bits 16]
    ; r9 : 'a [@bits 16]
    ; ra : 'a [@bits 16]
    ; rb : 'a [@bits 16]
    ; rc : 'a [@bits 16]
    ; rd : 'a [@bits 16]
    ; re : 'a [@bits 16]
    }
  [@@deriving hardcaml]

  let get t (reg : [< Register.real ]) =
    let { r1; r2; r3; r4; r5; r6; r7; r8; r9; ra; rb; rc; rd; re } = t in
    match reg with
    | `r1 -> r1
    | `r2 -> r2
    | `r3 -> r3
    | `r4 -> r4
    | `r5 -> r5
    | `r6 -> r6
    | `r7 -> r7
    | `r8 -> r8
    | `r9 -> r9
    | `ra -> ra
    | `rb -> rb
    | `rc -> rc
    | `rd -> rd
    | `re -> re
  ;;
end

module O = struct
  type 'a t =
    { io : 'a IO.t
    ; pc : 'a [@bits 16]
    ; registers : 'a Registers.t
    }
  [@@deriving hardcaml]
end

module Instr_parts = struct
  type 'a t =
    { op : 'a [@bits 4]
    ; rd : 'a [@bits 4]
    ; rs1 : 'a [@bits 4]
    ; rs2 : 'a [@bits 4]
    }
  [@@deriving hardcaml]

  let decode t = Of_signal.unpack ~rev:true t
end

module State = struct
  type t =
    | Boot
    | Fetch_instruction
    | Fetch_imm1
    | Fetch_imm2
    | Read
    | Write
  [@@deriving compare ~localize, enumerate, sexp_of]
end

let splits (type n) signal ~(widths : (int, n) Fixed_list.t) =
  let final_low, splits =
    Fixed_list.fold_map widths ~init:(width signal) ~f:(fun high width ->
      let low = high - width in
      low, signal.:[high - 1, low])
  in
  [%test_eq: int] 0 final_low;
  splits
;;

let op_mux value f =
  Instruction.Opcode.all
  |> List.sort ~compare:(Comparable.lift [%compare: int] ~f:Instruction.Opcode.to_int)
  |> List.map ~f
  |> mux value
;;

let op_switch value f =
  Always.switch
    value
    (Instruction.Opcode.all
     |> List.sort ~compare:(Comparable.lift [%compare: int] ~f:Instruction.Opcode.to_int)
     |> List.mapi ~f:(fun idx x -> of_unsigned_int ~width:4 idx, f x))
;;

let create scope (i : _ I.t) : _ O.t =
  let open Always in
  let reg_spec = Clocking.to_spec i.clocking in
  (* State *)
  let%hw.State_machine state =
    Always.State_machine.create (module State) reg_spec ~enable:i.enable
  in
  let%hw_var instr = Variable.cut_through_reg ~width:16 reg_spec ~enable:i.enable in
  let%hw_var imm1 = Variable.cut_through_reg ~width:16 reg_spec ~enable:i.enable in
  let%hw_var imm2 = Variable.cut_through_reg ~width:16 reg_spec ~enable:i.enable in
  let%hw_var pc = Variable.reg ~width:16 reg_spec ~enable:i.enable in
  let registers = Registers.Of_always.reg reg_spec ~enable:i.enable in
  let%hw.Instr_parts.Of_signal instr_parts = Instr_parts.decode instr.value in
  let%hw.IO.Of_always io = IO.Of_always.cut_through_reg reg_spec ~enable:i.enable in
  (* Wires *)
  let execute = Variable.wire ~default:gnd () in
  (* Calculations *)
  let [ op; rd; rs1; rs2 ] = splits instr.value ~widths:[ 4; 4; 4; 4 ] in
  let uses_rs1 =
    op_mux op (function
      | #Instruction.Op.arith -> vdd
      | `LDA -> vdd
      | `STO -> vdd
      | `BRANCH -> gnd
      | #Instruction.Op.jump -> vdd)
  in
  let needs_imm1 = uses_rs1 &: (rs1 ==:. 0xF) in
  let uses_rs2 =
    op_mux op (function
      | #Instruction.Op.arith -> vdd
      | `LDA -> gnd
      | `STO -> vdd
      | `BRANCH -> vdd
      | #Instruction.Op.jump -> vdd)
  in
  let needs_imm2 = uses_rs2 &: (rs2 ==:. 0xF) in
  let non_branch_next_pc =
    pc.value
    +: mux
         (needs_imm1 @: needs_imm2)
         [ of_string "16'd1"; of_string "16'd2"; of_string "16'd2"; of_string "16'd3" ]
  in
  (* Always *)
  let do_io ?data next_state ~addr =
    proc
      [ io.enable <-- vdd
      ; io.addr <-- addr
      ; io.write <-- of_bool (Option.is_some data)
      ; io.data <-- Option.value data ~default:(zero 16)
      ; state.set_next next_state
      ]
  in
  let fetch_next ?(next_pc = non_branch_next_pc) () =
    proc [ do_io Fetch_instruction ~addr:next_pc; pc <-- next_pc ]
  in
  let set_rd res =
    switch
      rd
      (Registers.to_list registers
       |> List.mapi ~f:(fun idx reg ->
         of_unsigned_int ~width:4 (idx + 1), [ reg <-- res ]))
  in
  let register_values_no_imm =
    zero 16 :: List.map (Registers.to_list registers) ~f:(fun r -> r.value)
  in
  let rs1_value = mux rs1 (register_values_no_imm @ [ imm1.value ]) in
  let rs2_value = mux rs2 (register_values_no_imm @ [ imm2.value ]) in
  compile
    [ io.enable <-- gnd
    ; state.switch
        [ Boot, [ fetch_next ~next_pc:i.init_pc () ]
        ; ( Fetch_instruction
          , [ instr <-- i.data_in
            ; if_ needs_imm1 [ do_io Fetch_imm1 ~addr:(pc.value +:. 1) ]
              @@ elif needs_imm2 [ do_io Fetch_imm2 ~addr:(pc.value +:. 1) ]
              @@ else_ [ execute <-- vdd ]
            ] )
        ; ( Fetch_imm1
          , [ imm1 <-- i.data_in
            ; if_ needs_imm2 [ do_io Fetch_imm2 ~addr:(pc.value +:. 2) ]
              @@ else_ [ execute <-- vdd ]
            ] )
        ; Fetch_imm2, [ imm2 <-- i.data_in; execute <-- vdd ]
        ; Read, [ set_rd i.data_in; fetch_next () ]
        ; Write, [ fetch_next () ]
        ]
    ; when_
        execute.value
        [ op_switch op (function
            | #Instruction.Op.arith as arith_op ->
              let result =
                match arith_op with
                | `ADD -> rs1_value +: rs2_value
                | `SUB -> rs1_value -: rs2_value
                | `MUL -> rs1_value *: rs2_value |> sel_bottom ~width:16
                | `SLU -> mux2 (rs1_value <: rs2_value) (one 16) (zero 16)
                | `SLT -> mux2 (rs1_value <+ rs2_value) (one 16) (zero 16)
                | `AND -> rs1_value &: rs2_value
                | `BOR -> rs1_value |: rs2_value
                | `XOR -> rs1_value ^: rs2_value
                | `SHL -> log_shift rs1_value ~f:sll ~by:(sel_bottom rs2_value ~width:4)
                | `SHR -> log_shift rs1_value ~f:srl ~by:(sel_bottom rs2_value ~width:4)
                | `ASR -> log_shift rs1_value ~f:sra ~by:(sel_bottom rs2_value ~width:4)
              in
              [ set_rd result; fetch_next () ]
            | `LDA -> [ do_io Read ~addr:(rs1_value +: uextend rs2 ~width:16) ]
            | `STO ->
              [ do_io Write ~addr:(rs1_value +: uextend rd ~width:16) ~data:rs2_value ]
            | `BRANCH ->
              (* top bit of [rd] picks the condition: set for [BGZ], clear for [BEZ] *)
              let taken = mux2 (msb instr_parts.rd) (rs2_value >+. 0) (rs2_value ==:. 0) in
              let branch_offset =
                sextend (lsbs instr_parts.rd @: instr_parts.rs1) ~width:16
              in
              let next_pc = mux2 taken (pc.value +: branch_offset) non_branch_next_pc in
              [ fetch_next ~next_pc () ]
            | #Instruction.Op.jump as op ->
              let next_pc =
                match op with
                | `JAL -> pc.value +: rs1_value +: rs2_value
                | `JLR -> rs1_value +: rs2_value
              in
              [ set_rd non_branch_next_pc; fetch_next ~next_pc () ])
        ]
    ];
  { io = { (IO.Of_always.value io) with enable = io.enable.value &: i.enable }
  ; pc = pc.value
  ; registers = Registers.Of_always.value registers
  }
;;

let hierachichal =
  let module H = Hierarchy.In_scope (I) (O) in
  fun ?instance scope input -> H.hierarchical ~scope ~name:"cpu" ?instance create input
;;
