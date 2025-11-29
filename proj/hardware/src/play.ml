open! Core
open! Hardcaml
open! Hardcaml.Signal

module Registers = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; reset : 'a
      ; read_reg_a : 'a [@bits 5]
      ; read_reg_b : 'a [@bits 5]
      ; write_enable : 'a
      ; data : 'a [@bits 32]
      ; write_reg : 'a [@bits 5]
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data_a : 'a [@bits 32]
      ; data_b : 'a [@bits 32]
      }
    [@@deriving hardcaml]
  end

  let create scope (i : _ I.t) : _ O.t =
    let ( -- ) = Scope.naming scope in
    let reg_spec = Reg_spec.create ~clock:i.clock ~reset:i.reset () in
    let regs =
      List.init 31 ~f:(fun idx ->
        let var = Always.Variable.reg reg_spec ~width:32 in
        let _ = var.value -- [%string "x%{idx + 1#Int}"] in
        var)
    in
    Always.(
      compile
        [ when_
            i.write_enable
            (List.mapi regs ~f:(fun idx reg ->
               when_ (i.write_reg ==:. idx + 1) [ reg <-- i.data ]))
        ]);
    let values = zero 32 :: List.map regs ~f:(fun r -> r.value) in
    { data_a = mux i.read_reg_b values; data_b = mux i.read_reg_b values }
  ;;
end

module ALU = struct
  module Op = struct
    type t =
      | Add
      | Sub
      | Xor
      | Or
      | And
      | Shift_left
      | Logical_shift_right
      | Arithmetic_shift_right
      | Set_less_than_signed
      | Set_less_than_unsigned
    [@@deriving sexp, compare ~localize, enumerate]

    include functor Enum.Make_enums
  end

  let execute' (type s) (module C : Comb.S with type t = s) ~a ~b ~op =
    let open C in
    let shift f = log_shift a ~by:(sel_bottom b ~width:5) ~f in
    let execute_op : Op.t -> _ = function
      | Add -> a +: b
      | Sub -> a -: b
      | Xor -> a ^: b
      | Or -> a |: b
      | And -> a &: b
      | Shift_left -> shift sll
      | Logical_shift_right -> shift srl
      | Arithmetic_shift_right -> shift sra
      | Set_less_than_signed -> mux2 (a <+ b) (one 32) (zero 32)
      | Set_less_than_unsigned -> mux2 (a <: b) (one 32) (zero 32)
    in
    Op.Binary.match_ (module C) op (List.map Op.all ~f:(fun op -> op, execute_op op))
  ;;

  let execute_bits = execute' (module Bits)
  let execute = execute' (module Signal)
end

let op_switch ~(here : [%call_pos]) cases ~on ~default =
  List.fold_right
    cases
    ~f:(fun (n, then_) rest -> Always.if_ ~here (on ==:. n) then_ [ rest ])
    ~init:(Always.proc ~here default)
;;

module CPU = struct
  module Size = struct
    type t =
      | Byte
      | Half
      | Word
    [@@deriving sexp, compare ~localize, enumerate]
  end

  module IO = struct
    type t =
      | Nop
      | Read of Size.t
      | Write of Size.t
      | Break
      | Call
    [@@deriving sexp, compare ~localize, enumerate]

    include functor Enum.Make_enums
  end

  module I = struct
    type 'a t =
      { clocking : 'a Clocking.t
      ; data : 'a [@bits 32]
      ; ready : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { data : 'a [@bits 32]
      ; addr : 'a [@bits 32]
      ; io : 'a IO.Binary.t
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | Startup
      | Reading_instr
      | Doing_IO
    [@@deriving sexp, compare ~localize, enumerate]
  end

  module IO_op = struct
    type t =
      | Read of
          { size : Size.t
          ; addr : Signal.t
          }
      | Write of
          { size : Size.t
          ; addr : Signal.t
          ; data : Signal.t
          }
      | Break
      | Call

    let assign var t =
      let io : IO.t =
        match t with
        | Read { size; _ } -> Read size
        | Write { size; _ } -> Write size
        | Break -> Break
        | Call -> Call
      in
      let addr =
        match t with
        | Read { addr; _ } | Write { addr; _ } -> addr
        | _ -> zero 32
      in
      let data =
        match t with
        | Write { data; _ } -> data
        | _ -> zero 32
      in
      O.Of_always.assign var { io = IO.Binary.Of_signal.of_enum io; addr; data }
    ;;
  end

  let create scope (i : _ I.t) =
    let state =
      Always.State_machine.create (module State) (Clocking.to_spec i.clocking)
    in
    let%hw.O.Of_always io = O.Of_always.wire Signal.zero in
    let%hw_var pc = Clocking.Var.reg i.clocking ~width:32 in
    let%hw_var instr = Clocking.Var.cut_through_reg i.clocking ~width:32 in
    let next_instr =
      Always.(
        proc
          [ IO_op.assign io (Read { size = Word; addr = pc.value })
          ; state.set_next Reading_instr
          ])
    in
    let _do_io op = Always.(proc [ IO_op.assign io op; state.set_next Doing_IO ]) in
    Always.(
      compile
        [ if_
            (state.is Startup)
            [ next_instr ]
            [ when_
                i.ready
                [ when_ (state.is Reading_instr) [ instr <-- i.data ]
                ; (let opcode = instr.value.:[6, 0] in
                   op_switch ~on:opcode [ 0b00_000_11, [] ] ~default:[])
                ]
            ]
        ])
  ;;
  (* SOMEDAY: finish *)
end
