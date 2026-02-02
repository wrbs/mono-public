open! Core
open! Hardcaml
open! Signal
open Cpu_types

module Registers = struct
  type 'a t =
    { a : 'a [@bits 8]
    ; x : 'a [@bits 8]
    ; y : 'a [@bits 8]
    ; s : 'a [@bits 8]
    }
  [@@deriving hardcaml]

  let get' { a; x; y; s } (reg : [< Reg.t ]) =
    match reg with
    | `A -> a
    | `X -> x
    | `Y -> y
    | `S -> s
  ;;

  let get t reg = Always.Variable.value (get' t reg)
  let set t reg value = Always.(get' t reg <-- value)
end

module Flags = struct
  type 'a t =
    { n : 'a
    ; v : 'a
    ; d : 'a
    ; i : 'a
    ; z : 'a
    ; c : 'a
    }
  [@@deriving hardcaml]

  let to_byte (type comb) (module C : Comb.S with type t = comb) { n; v; d; i; z; c } =
    let open C in
    concat_msb [ n; v; vdd; gnd; d; i; z; c ]
  ;;

  let of_byte (type comb) (module C : Comb.S with type t = comb) x =
    let open C in
    { n = x.:(7); v = x.:(6); d = x.:(3); i = x.:(2); z = x.:(1); c = x.:(0) }
  ;;

  let get' { n; v; d; i; z; c } (flag : Flag.t) =
    match flag with
    | N -> n
    | V -> v
    | D -> d
    | I -> i
    | Z -> z
    | C -> c
  ;;

  let get t reg = Always.Variable.value (get' t reg)
  let set t reg value = Always.(get' t reg <-- value)
  let get_byte t = map t ~f:Always.Variable.value |> to_byte (module Signal)
  let set_byte t byte = Of_always.(t <-- of_byte (module Signal) byte)
end

module I = struct
  type 'a t =
    { enable : 'a
    ; irq : 'a
    ; nmi : 'a
    ; data : 'a [@bits 8]
    ; clock : 'a
    ; clear : 'a
    }
  [@@deriving hardcaml, typed_fields]
end

module Mem_port = struct
  type 'a t =
    { addr : 'a [@bits 16]
    ; data : 'a [@bits 8]
    ; write : 'a
    }
  [@@deriving hardcaml]

  let create () =
    let vars = Of_always.wire zero in
    let read addr = Always.(vars.addr <-- addr) in
    let write addr data =
      Always.(proc [ vars.write <-- vdd; vars.addr <-- addr; vars.data <-- data ])
    in
    let t = map vars ~f:Always.Variable.value in
    t, ~read, ~write
  ;;
end

module Interrupt_type = struct
  module Cases = struct
    type t =
      | RESET
      | BRK
      | NMI
      | IRQ
    [@@deriving sexp_of, compare ~localize, enumerate]
  end

  module Binary = Enum.Make_binary (Cases)
  include Cases

  let set t case = Binary.Of_always.assign t (Binary.Of_signal.of_enum case)

  let _actually_push = function
    | RESET -> false
    | BRK | NMI | IRQ -> true
  ;;

  let vector_low' = function
    | BRK | IRQ -> 0xFFFE
    | RESET -> 0xFFFC
    | NMI -> 0xFFFA
  ;;

  let vector_high' t = vector_low' t + 1
  let vector_low t = of_unsigned_int (vector_low' t) ~width:16
  let vector_high t = of_unsigned_int (vector_high' t) ~width:16

  let match_ var ~f =
    Binary.Of_signal.match_
      (Binary.Of_always.value var)
      (List.map all ~f:(fun t -> t, f t))
  ;;
end

module State = struct
  type t =
    | Reset
    | Illegal
    | Fetch (* fetch new opcode *)
    | Decode (* handle fetched opcode *)
    (* Generic execution: *)
    | Perform_read (* do a read op *)
    | Perform_write (* do a write op *)
    | Perform_read_write1 (* read rw op value *)
    | Perform_read_write2 (* write back read value, nxt=write *)
    (* Addressing modes *)
    | Absolute
    | Zero_page_indexed
    | Absolute_indexed
    | Indexed_fix_cycle
    | Indirect_X1
    | Indirect_X2
    | Indirect_X3
    | Indirect_Y1
    | Indirect_Y2
    (* Branching *)
    | Branch_taken
    | Branch_fix_pch
    (* Special instructions *)
    | JMP
    | JMPI1
    | JMPI2
    | JMPI3
    | JSR1
    | JSR2
    | JSR3
    | JSR4
    | RTS1
    | RTS2
    | RTS3
    | RTS4
    | RTI1
    | RTI2
    | RTI3
    | RTI4
    | PHA
    | PHP
    | PLA1
    | PLA2
    | PLP1
    | PLP2
    (* Interrupts:  *)
    | Interrupt0
    | Interrupt1
    | Interrupt2
    | Interrupt3
    | Interrupt4
    | Interrupt5
  [@@deriving sexp_of, compare ~localize, enumerate]

  let bits = num_bits_to_represent (List.length all)
end

module O = struct
  type 'a t =
    { mem : 'a Mem_port.t
    ; state : 'a [@bits State.bits]
    ; fetching : 'a
    ; pc : 'a [@bits 16]
    ; a : 'a [@bits 8]
    ; s : 'a [@bits 8]
    ; x : 'a [@bits 8]
    ; y : 'a [@bits 8]
    ; p : 'a [@bits 8]
    ; illegal : 'a
    }
  [@@deriving hardcaml]
end

let match_op_default op ~f ~default =
  Op.Binary.Of_signal.match_
    ~default
    (Op.Binary.Of_always.value op)
    (List.filter_map Op.all ~f:(fun op ->
       let%map.Option x = f op in
       op, x))
;;

let match_op op ~f =
  Op.Binary.Of_signal.match_
    (Op.Binary.Of_always.value op)
    (List.map Op.all ~f:(fun op -> op, f op))
;;

let switch_op op ~f =
  Op.Binary.Of_always.match_
    ~default:[]
    (Op.Binary.Of_always.value op)
    (List.filter_map Op.all ~f:(fun op ->
       match f op with
       | [] -> None
       | action -> Some (op, action)))
;;

let adc a b ~c_in =
  [%test_eq: int] (width a) (width b);
  let big = Unsigned.(a +: b +: c_in) in
  let c = sel_top big ~width:2 |> any_bit_set in
  let result = sel_bottom big ~width:(width a) in
  let v = ~:(msb a ^: msb b) &: msb a ^: msb result in
  result, ~c, ~v
;;

let sub_carry a b =
  let full = Unsigned.(~:b +: vdd) +: (gnd @: a) in
  let res = sel_bottom full ~width:8 in
  res, ~carry:~:full.:(8)
;;

let set_zn' ~(flags : Always.Variable.t Flags.t) res =
  Always.(proc [ flags.z <-- no_bits_set res; flags.n <-- res.:(7) ])
;;

let execute_op
  scope
  ~enable
  ~op
  ~operand
  ~result
  ~(regs : Always.Variable.t Registers.t)
  ~(flags : Always.Variable.t Flags.t)
  ~h
  =
  let open Always in
  let%hw and_ = regs.a.value &: operand in
  let set_zn res = set_zn' ~flags res in
  let do_adc ?(c_in = flags.c.value) op =
    let result, ~c, ~v = adc regs.a.value op ~c_in in
    [ set_zn result; regs.a <-- result; flags.c <-- c; flags.v <-- v ]
  in
  when_
    enable
    [ switch_op op ~f:(function
        | Read NOP -> []
        | Read ADC -> do_adc operand
        | Read AND -> [ regs.a <-- and_; set_zn and_ ]
        | Read BIT ->
          [ flags.z <-- no_bits_set and_
          ; flags.n <-- operand.:(7)
          ; flags.v <-- operand.:(6)
          ]
        | Read (CP r) ->
          let res, ~carry = sub_carry (Registers.get regs r) operand in
          [ set_zn res; flags.c <-- ~:carry ]
        | Read EOR ->
          let res = regs.a.value ^: operand in
          [ set_zn res; regs.a <-- res ]
        | Read LAX -> [ set_zn operand; regs.a <-- operand; regs.x <-- operand ]
        | Read (LD r) -> [ set_zn operand; Registers.set regs r operand ]
        | Read ORA ->
          let res = regs.a.value |: operand in
          [ set_zn res; regs.a <-- res ]
        | Read SBC -> do_adc ~:operand
        | Read ANC -> [ regs.a <-- and_; set_zn and_; flags.c <-- and_.:(7) ]
        | Read ALR ->
          (* and then lsr *)
          let res = srl and_ ~by:1 in
          [ regs.a <-- res; set_zn res ]
        | Read ARR ->
          (* and then ror I guess but just odd *)
          let res = srl and_ ~by:1 |: flags.c.value @: zero 7 in
          [ regs.a <-- res
          ; set_zn res
          ; flags.c <-- res.:(6)
          ; flags.v <-- res.:(6) ^: res.:(5)
          ]
        | Read AXS ->
          (* I don't even know how to justify but passes tests *)
          let res, ~carry = sub_carry (regs.a.value &: regs.x.value) operand in
          [ regs.x <-- res; set_zn res; flags.c <-- ~:carry ]
        | Write (ST reg) -> [ result <-- Registers.get regs reg ]
        | Write SAX -> [ result <-- (regs.a.value &: regs.x.value) ]
        | Read_write ASL ->
          [ result <-- sll operand ~by:1; flags.c <-- operand.:(7); set_zn result.value ]
        | Read_write DEC -> [ result <-- Signal.decr operand; set_zn result.value ]
        | Read_write INC -> [ result <-- Signal.incr operand; set_zn result.value ]
        | Read_write LSR ->
          [ result <-- srl operand ~by:1; flags.c <-- operand.:(0); set_zn result.value ]
        | Read_write ROL ->
          [ result <-- (sll operand ~by:1 |: uextend flags.c.value ~width:8)
          ; flags.c <-- operand.:(7)
          ; set_zn result.value
          ]
        | Read_write ROR ->
          [ result <-- (srl operand ~by:1 |: flags.c.value @: zero 7)
          ; flags.c <-- operand.:(0)
          ; set_zn result.value
          ]
        | Read_write DCP ->
          (* dec then cpa *)
          let dec = Signal.decr operand in
          let res, ~carry = sub_carry regs.a.value dec in
          [ result <-- dec; flags.c <-- ~:carry; set_zn res ]
        | Read_write ISC ->
          (* inc then sbc *)
          let inc = Signal.incr operand in
          [ proc (do_adc ~:inc); result <-- inc ]
        | Read_write SLO ->
          (* asl then ora *)
          let asl = sll operand ~by:1 in
          let ora = regs.a.value |: asl in
          [ result <-- asl; flags.c <-- operand.:(7); regs.a <-- ora; set_zn ora ]
        | Read_write RLA ->
          (* rol then and *)
          let rol = sll operand ~by:1 |: uextend flags.c.value ~width:8 in
          let and_ = rol &: regs.a.value in
          [ result <-- rol; flags.c <-- operand.:(7); regs.a <-- and_; set_zn and_ ]
        | Read_write SRE ->
          (* lsr then eor *)
          let lsr_ = srl operand ~by:1 in
          let eor = regs.a.value ^: lsr_ in
          [ result <-- lsr_; flags.c <-- operand.:(0); regs.a <-- eor; set_zn eor ]
        | Read_write RRA ->
          (* ror then adc *)
          let ror = srl operand ~by:1 |: flags.c.value @: zero 7 in
          [ proc (do_adc ror ~c_in:operand.:(0)); result <-- ror ]
        | SH _ -> [ result <-- h ])
    ]
;;

let op_kind scope ~op =
  let bits =
    match_op op ~f:(function
      | Read _ -> of_bit_string "100"
      | Write _ | SH _ -> of_bit_string "010"
      | Read_write _ -> of_bit_string "001")
  in
  let%hw r = bits.:(2) in
  let%hw w = bits.:(1) in
  let%hw rw = bits.:(0) in
  ~r, ~w, ~rw
;;

let create scope (i : _ I.t) : _ O.t =
  let enable = i.enable in
  let open Always in
  let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
  (* Internal state *)
  let%hw.State_machine state =
    Always.State_machine.create (module State) reg_spec ~enable
  in
  let%hw.Interrupt_type.Binary.Of_always interrupt_type =
    let init = Interrupt_type.Binary.Of_bits.of_enum RESET in
    Interrupt_type.Binary.Of_always.reg
      reg_spec
      ~initialize_to:init
      ~reset_to:init
      ~enable
  in
  (* Registers *)
  let%hw_var pc = Variable.reg ~width:16 reg_spec ~enable in
  let%hw.Registers.Of_always regs = Registers.Of_always.reg reg_spec ~enable in
  let%hw.Flags.Of_always flags = Flags.Of_always.reg reg_spec ~enable in
  let set_zn res = set_zn' ~flags res in
  (* Memory *)
  let mem_port, ~read, ~write = Mem_port.create () in
  let%hw.Mem_port.Of_signal mem_port in
  (* Instruction *)
  let%hw_var instr = Variable.reg ~width:8 reg_spec ~enable in
  let%hw.Op.Binary.Of_always op = Op.Binary.Of_always.reg reg_spec ~enable in
  let ~r, ~w, ~rw = op_kind scope ~op in
  (* Regs for addr to read from *)
  let%hw_var addr_high = Variable.reg ~width:8 reg_spec ~enable in
  let%hw_var addr_low = Variable.reg ~width:8 reg_spec ~enable in
  let%hw effective_addr = addr_high.value @: addr_low.value in
  (* Shared execution logic *)
  let%hw_var operand = Variable.wire ~default:i.data () in
  let%hw_var trigger_execute = Variable.wire ~default:gnd () in
  let%hw_var execute_unit_enable = Variable.wire ~default:gnd () in
  let%hw_var execute_result = Variable.cut_through_reg ~width:8 reg_spec ~enable in
  let start_execute = trigger_execute <-- vdd in
  (* Flag get + set *)
  (* Random helper vars*)
  let%hw_var h = Variable.reg ~width:8 reg_spec ~enable (* todo: can I use tmp? *) in
  let%hw_var index_reg = Variable.reg ~width:8 reg_spec ~enable in
  let%hw_var pointer = Variable.reg ~width:8 reg_spec ~enable in
  let%hw_var index_base = Variable.reg ~width:8 reg_spec ~enable in
  let%hw_var index_carry = Variable.cut_through_reg ~width:1 reg_spec ~enable in
  let%hw stack_addr = of_unsigned_int ~width:8 0x01 @: regs.s.value in
  let start_indexed_generic h_value =
    let add_full = Unsigned.(addr_low.value +: index_reg.value) in
    proc
      [ h <-- h_value
      ; addr_low <-- sel_bottom add_full ~width:8
      ; index_carry <-- add_full.:(8)
      ; if_
          (r &: ~:(index_carry.value))
          [ start_execute ]
          [ state.set_next Indexed_fix_cycle ]
      ]
  in
  compile
    [ (* Instantiate the execution unit *)
      execute_op
        scope
        ~enable:execute_unit_enable.value
        ~op
        ~operand:operand.value
        ~result:execute_result
        ~regs
        ~flags
        ~h:h.value
    ; when_
        trigger_execute.value
        [ when_ r [ state.set_next Perform_read ]
        ; when_ w [ execute_unit_enable <-- vdd; state.set_next Perform_write ]
        ; when_ rw [ state.set_next Perform_read_write1 ]
        ]
    ; state.switch
        [ ( Reset
          , [ read pc.value
            ; Interrupt_type.set interrupt_type RESET
            ; state.set_next Interrupt0
            ] )
        ; Illegal, [ read (uextend ~width:16 instr.value) ]
        ; ( Fetch
          , [ read pc.value
            ; incr pc
            ; instr <-- i.data
            ; switch
                i.data
                (Decoded.switch (fun decoded ->
                   match Decoded.op decoded with
                   | Some new_op ->
                     [ Op.Binary.(Of_always.assign op (Of_signal.of_enum new_op)) ]
                   | None -> []))
            ; state.set_next Decode
            ] )
        ; ( Decode
          , [ read pc.value
            ; switch
                instr.value
                (Decoded.switch (function
                  | Illegal -> [ state.set_next Illegal ]
                  | Immediate _ ->
                    [ incr pc; execute_unit_enable <-- vdd; state.set_next Fetch ]
                  | Accumulator _ ->
                    [ operand <-- regs.a.value
                    ; execute_unit_enable <-- vdd
                    ; regs.a <-- execute_result.value
                    ; state.set_next Fetch
                    ]
                  | Absolute _ ->
                    [ incr pc; addr_low <-- i.data; state.set_next Absolute ]
                  | Zero_page _ ->
                    [ incr pc; addr_high <-- zero 8; addr_low <-- i.data; start_execute ]
                  | Zero_page_indexed (_, r) ->
                    [ incr pc
                    ; addr_high <-- zero 8
                    ; addr_low <-- i.data
                    ; index_reg <-- Registers.get regs r
                    ; state.set_next Zero_page_indexed
                    ]
                  | Absolute_indexed (_, r) ->
                    [ incr pc
                    ; addr_low <-- i.data
                    ; index_reg <-- Registers.get regs r
                    ; state.set_next Absolute_indexed
                    ]
                  | Indirect_X _ ->
                    [ incr pc; pointer <-- i.data; state.set_next Indirect_X1 ]
                  | Indirect_Y _ ->
                    [ incr pc
                    ; pointer <-- i.data
                    ; index_reg <-- regs.y.value
                    ; state.set_next Indirect_Y1
                    ]
                  | Nop -> [ state.set_next Fetch ]
                  | Inc r ->
                    let res = Signal.incr (Registers.get regs r) in
                    [ set_zn res; Registers.set regs r res; state.set_next Fetch ]
                  | Dec r ->
                    let res = Signal.decr (Registers.get regs r) in
                    [ set_zn res; Registers.set regs r res; state.set_next Fetch ]
                  | Transfer (r1, r2) ->
                    [ Registers.set regs r2 (Registers.get regs r1)
                    ; proc
                        (match r2 with
                         | `S -> []
                         | `A | `X | `Y -> [ set_zn (Registers.get regs r1) ])
                    ; state.set_next Fetch
                    ]
                  | Set f -> [ Flags.set flags f vdd; state.set_next Fetch ]
                  | Clear f -> [ Flags.set flags f gnd; state.set_next Fetch ]
                  | Branch (flag, value) ->
                    [ incr pc
                    ; if_
                        (Flags.get flags flag ==: of_bool value)
                        [ pointer <-- i.data; state.set_next Branch_taken ]
                        [ state.set_next Fetch ]
                    ]
                  | JMP -> [ incr pc; pointer <-- i.data; state.set_next JMP ]
                  | JMPI -> [ incr pc; addr_low <-- i.data; state.set_next JMPI1 ]
                  | JSR -> [ incr pc; addr_low <-- i.data; state.set_next JSR1 ]
                  | RTS -> [ state.set_next RTS1 ]
                  | RTI -> [ state.set_next RTI1 ]
                  | PHA -> [ state.set_next PHA ]
                  | PHP -> [ state.set_next PHP ]
                  | PLA -> [ state.set_next PLA1 ]
                  | PLP -> [ state.set_next PLP1 ]
                  | BRK ->
                    [ Interrupt_type.set interrupt_type BRK; state.set_next Interrupt1 ]))
            ] )
        ; ( Perform_read
          , [ read effective_addr; execute_unit_enable <-- vdd; state.set_next Fetch ] )
        ; ( Perform_write
          , [ write effective_addr execute_result.value; state.set_next Fetch ] )
        ; Perform_read_write1, [ read effective_addr; state.set_next Perform_read_write2 ]
        ; ( Perform_read_write2
          , [ write effective_addr i.data
            ; execute_unit_enable <-- vdd
            ; state.set_next Perform_write
            ] )
        ; Absolute, [ read pc.value; incr pc; addr_high <-- i.data; start_execute ]
        ; ( Zero_page_indexed
          , [ read effective_addr
            ; addr_low <-- addr_low.value +: index_reg.value
            ; start_execute
            ] )
        ; (let h_and =
             match_op_default op ~default:(ones 8) ~f:(function
               | SH `A -> Some (regs.a.value &: regs.x.value)
               | SH `X -> Some regs.x.value
               | SH `Y -> Some regs.y.value
               | _ -> None)
           in
           ( Absolute_indexed
           , [ read pc.value
             ; incr pc
             ; addr_high <-- i.data
             ; start_indexed_generic (Signal.incr i.data &: h_and)
             ] ))
        ; ( Indexed_fix_cycle
          , [ read effective_addr
            ; when_ index_carry.value [ addr_high <-- h.value ]
            ; start_execute
            ] )
        ; ( Indirect_X1
          , [ read (zero 8 @: pointer.value)
            ; pointer <-- pointer.value +: regs.x.value
            ; state.set_next Indirect_X2
            ] )
        ; ( Indirect_X2
          , [ read (zero 8 @: pointer.value)
            ; addr_low <-- i.data
            ; incr pointer
            ; state.set_next Indirect_X3
            ] )
        ; ( Indirect_X3
          , [ read (zero 8 @: pointer.value); addr_high <-- i.data; start_execute ] )
        ; ( Indirect_Y1
          , [ read (zero 8 @: pointer.value)
            ; addr_low <-- i.data
            ; index_base <-- i.data
            ; incr pointer
            ; state.set_next Indirect_Y2
            ] )
        ; ( Indirect_Y2
          , [ read (zero 8 @: pointer.value)
            ; addr_high <-- i.data
            ; start_indexed_generic (Signal.incr i.data)
            ] )
        ; (let dest = pc.value +: sextend pointer.value ~width:16 in
           let pch_needs_fix = sel_top dest ~width:8 <>: sel_top pc.value ~width:8 in
           ( Branch_taken
           , [ read pc.value
             ; pc <-- dest
             ; if_
                 pch_needs_fix
                 [ pointer <-- sel_top pc.value ~width:8; state.set_next Branch_fix_pch ]
                 [ state.set_next Fetch ]
             ] ))
        ; ( Branch_fix_pch
          , [ read (pointer.value @: sel_bottom pc.value ~width:8); state.set_next Fetch ]
          )
        ; JMP, [ read pc.value; pc <-- i.data @: pointer.value; state.set_next Fetch ]
        ; JMPI1, [ read pc.value; incr pc; addr_high <-- i.data; state.set_next JMPI2 ]
        ; ( JMPI2
          , [ read effective_addr
            ; incr addr_low
            ; pointer <-- i.data
            ; state.set_next JMPI3
            ] )
        ; ( JMPI3
          , [ read effective_addr; pc <-- i.data @: pointer.value; state.set_next Fetch ]
          )
        ; JSR1, [ read stack_addr; state.set_next JSR2 ]
        ; ( JSR2
          , [ write stack_addr (sel_top pc.value ~width:8)
            ; decr regs.s
            ; state.set_next JSR3
            ] )
        ; ( JSR3
          , [ write stack_addr (sel_bottom pc.value ~width:8)
            ; decr regs.s
            ; state.set_next JSR4
            ] )
        ; JSR4, [ read pc.value; pc <-- i.data @: addr_low.value; state.set_next Fetch ]
        ; RTS1, [ read stack_addr; incr regs.s; state.set_next RTS2 ]
        ; RTS2, [ read stack_addr; incr regs.s; pointer <-- i.data; state.set_next RTS3 ]
        ; RTS3, [ read stack_addr; pc <-- i.data @: pointer.value; state.set_next RTS4 ]
        ; RTS4, [ incr pc; state.set_next Fetch ]
        ; RTI1, [ read stack_addr; incr regs.s; state.set_next RTI2 ]
        ; ( RTI2
          , [ read stack_addr
            ; incr regs.s
            ; Flags.set_byte flags i.data
            ; state.set_next RTI3
            ] )
        ; RTI3, [ read stack_addr; incr regs.s; pointer <-- i.data; state.set_next RTI4 ]
        ; RTI4, [ read stack_addr; pc <-- i.data @: pointer.value; state.set_next Fetch ]
        ; PHA, [ write stack_addr regs.a.value; decr regs.s; state.set_next Fetch ]
        ; ( PHP
          , [ write stack_addr (Flags.get_byte flags |: of_unsigned_int ~width:8 0x10)
            ; decr regs.s
            ; state.set_next Fetch
            ] )
        ; PLA1, [ read stack_addr; incr regs.s; state.set_next PLA2 ]
        ; ( PLA2
          , [ read stack_addr; regs.a <-- i.data; set_zn i.data; state.set_next Fetch ] )
        ; PLP1, [ read stack_addr; incr regs.s; state.set_next PLP2 ]
        ; PLP2, [ read stack_addr; Flags.set_byte flags i.data; state.set_next Fetch ]
        ; Interrupt0, [ read pc.value; state.set_next Interrupt1 ]
        ; ( Interrupt1
          , [ write stack_addr (sel_top pc.value ~width:8)
            ; decr regs.s
            ; state.set_next Interrupt2
            ] )
        ; ( Interrupt2
          , [ write stack_addr (sel_bottom pc.value ~width:8)
            ; decr regs.s
            ; state.set_next Interrupt3
            ] )
        ; ( Interrupt3
          , [ write stack_addr (Flags.get_byte flags)
            ; flags.i <-- vdd
            ; decr regs.s
            ; state.set_next Interrupt4
            ] )
        ; ( Interrupt4
          , [ read (Interrupt_type.match_ interrupt_type ~f:Interrupt_type.vector_low)
            ; pc <-- (i.data |> uextend ~width:16)
            ; state.set_next Interrupt5
            ] )
        ; ( Interrupt5
          , [ read (Interrupt_type.match_ interrupt_type ~f:Interrupt_type.vector_high)
            ; pc <-- (pc.value |: i.data @: zero 8)
            ; state.set_next Fetch
            ] )
        ]
    ];
  { mem = mem_port
  ; fetching = state.is Fetch
  ; pc = pc.value
  ; a = regs.a.value
  ; s = regs.s.value
  ; x = regs.x.value
  ; y = regs.y.value
  ; p = Flags.get_byte flags
  ; state = state.current
  ; illegal = state.is Illegal
  }
;;

let hierarchical (scope : Scope.t) (input : Signal.t I.t) =
  let module H = Hierarchy.In_scope (I) (O) in
  H.hierarchical ~scope ~name:"cpu" create input
;;
