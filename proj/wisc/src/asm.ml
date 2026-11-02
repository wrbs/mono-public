open! Core
open Ints

let builtin_reg reg =
  let c = String.nget (Register.to_string reg) 1 in
  Asm_ast.Register.of_string ("%" ^ Char.to_string c)
;;

module IR = struct
  module Expr = struct
    type t =
      | Sub of t * t
      | Constant of int
      | Label of Asm_ast.Label.Ref.t
  end

  module Unresolved_instruction = struct
    type t =
      | Arith of
          { op : Instruction.Op.arith
          ; rd : Register.t
          ; rs1 : Register.t
          ; rs2 : Register.t
          }
      | LDA of
          { rd : Register.t
          ; addr : Register.t
          ; offset : Expr.t
          }
      | STO of
          { addr : Register.t
          ; rs : Register.t
          ; offset : Expr.t
          }
      | Branch of
          { op : Instruction.Op.branch
          ; rs : Register.t
          ; offset : Expr.t
          }
      | Jump of
          { op : Instruction.Op.jump
          ; rd : Register.t
          ; rs1 : Register.t
          ; rs2 : Register.t
          }
  end

  module Elem = struct
    type t =
      | Instruction of Unresolved_instruction.t
      | Data of Expr.t
      | Local_label of int
  end

  module Op = struct
    (* normalized to lowercase *)
    type base = Instruction.Op.t

    let base_of_string s = [%of_string: Instruction.Op.t] (String.uppercase s)

    type t =
      [ base
      | `dat
      | `alias
      | `define
      | (* pseudo-instructions *)
        `mov
      | `neg
      | `not
      | `sgu
      | `sgt
      | `sle
      | `sge
      | `sleu
      | `sgeu
      | `seq
      | `sne
      | `bnz
      | `blez
      | `j
      | `jr
      | `nop
      ]
    [@@deriving of_string]
  end
end

module Phase_1 = struct
  type t =
    { aliases : Register.t Asm_ast.Register.Table.t
    ; global_labels : IR.Expr.t Asm_ast.Label.Name.Table.t
          (* label definitions are [Constant pos]; [define]s are expanded in phase 2 *)
    ; local_labels : int Queue.t Int.Table.t
    ; mutable pos : int
    ; ir : IR.Elem.t Queue.t
    }

  let init () =
    let aliases = Asm_ast.Register.Table.create () in
    List.iter Register.all ~f:(fun reg ->
      Hashtbl.add_exn aliases ~key:(builtin_reg reg) ~data:reg);
    { aliases
    ; global_labels = Asm_ast.Label.Name.Table.create ()
    ; local_labels = Int.Table.create ()
    ; pos = 0
    ; ir = Queue.create ()
    }
  ;;

  module Op_shape = struct
    type (_, _) base =
      | Register_only : (Register.t, [ `has_default | `req ]) base
      | Any : (Register.t * IR.Expr.t or_null, [ `has_default | `req ]) base
      | Expr : (IR.Expr.t, [ `has_default | `req ]) base
      | Parsed_reg : (Asm_ast.Register.t, [ `req ]) base
      | Parsed_any : (Asm_ast.Operand.t, [ `req ]) base
      | Label_name : (Asm_ast.Label.Name.t, [ `req ]) base
      | Optional : ('a, [> `has_default | `req ]) base -> ('a, [ `opt ]) base
      | Rest : ('a, [> `req ]) base -> ('a list, [ `opt ]) base

    type 'a is_req = Is_req : ('a, [> `req ]) base -> 'a is_req

    type 'a t =
      | [] : unit t
      | ( :: ) : ('a, _) base * 'b t -> ('a -> 'b) t

    let get_required (type a) t (shape : (a, _) base) (operand : Asm_ast.Operand.t) : a =
      let (Is_req shape) = Is_req shape in
      let lookup_reg reg = Hashtbl.find_exn t.aliases reg in
      match shape, operand with
      | Parsed_any, x -> x
      | Register_only, Register reg -> lookup_reg reg
      | Register_only, (Constant _ | Label _) -> failwith "expected register"
      | Any, Register reg -> lookup_reg reg, Null
      | Any, Constant const -> `rf, This (Constant const)
      | Any, Label l -> `rf, This (Label l)
      | Expr, Register _ -> failwith "expected expression"
      | Expr, Constant const -> Constant const
      | Expr, Label l -> Label l
      | Parsed_reg, Register reg -> reg
      | Parsed_reg, (Constant _ | Label _) -> failwith "expected register"
      | Label_name, Label (Global name) -> name
      | Label_name, (Register _ | Constant _ | Label (Local _)) ->
        failwith "expected label name"
    ;;

    let rec get_base
      : type a kind.
        _ -> (a, kind) base -> Asm_ast.Operand.t list -> a * Asm_ast.Operand.t list
      =
      fun t shape operands ->
      match shape, operands with
      | Parsed_any, op :: rest -> get_required t Parsed_any op, rest
      | Register_only, op :: rest -> get_required t Register_only op, rest
      | Any, op :: rest -> get_required t Any op, rest
      | Expr, op :: rest -> get_required t Expr op, rest
      | Parsed_reg, op :: rest -> get_required t Parsed_reg op, rest
      | Label_name, op :: rest -> get_required t Label_name op, rest
      | Optional inner, [] ->
        let default : Asm_ast.Operand.t =
          match inner with
          | Expr -> Constant 0
          | _ -> Register (Asm_ast.Register.of_string "%0")
        in
        get_required t inner default, []
      | Optional inner, _ :: _ -> get_base t inner operands
      | Rest inner, _ -> List.map operands ~f:(fun op -> get_required t inner op), []
      | _, [] -> failwith "Too few operands"
    ;;

    let rec get : type a. _ -> a t -> Asm_ast.Operand.t list -> a Heterogeneous_list.t =
      fun ctx t operands ->
      match t with
      | [] ->
        (match operands with
         | [] -> []
         | _ -> failwith "too many operands")
      | base :: rest ->
        let x, operands' = get_base ctx base operands in
        x :: get ctx rest operands'
    ;;
  end

  let add_ir t elem =
    Queue.enqueue t.ir elem;
    match elem with
    | Instruction _ | Data _ -> t.pos <- t.pos + 1
    | Local_label _ -> ()
  ;;

  let rec add_instruction t (op : IR.Op.t) operands =
    let decode shape = Op_shape.get t shape operands in
    match op with
    (* special ops *)
    | `dat ->
      let [ hd; tl ] = decode [ Expr; Rest Expr ] in
      add_ir t (Data hd);
      List.iter tl ~f:(fun e -> add_ir t (Data e))
    | `alias ->
      let [ alias; underlying ] = decode [ Parsed_reg; Register_only ] in
      Hashtbl.add_exn t.aliases ~key:alias ~data:underlying
    | `define ->
      let [ name; value ] = decode [ Label_name; Expr ] in
      (match value with
       | Label (Local _) -> failwith "local labels can't be used in a define"
       | Sub _ | Constant _ | Label (Global _) -> ());
      Hashtbl.add_exn t.global_labels ~key:name ~data:value
      (* Base ops *)
    | #Instruction.Op.arith as op ->
      let [ rd; (rs1, e1); (rs2, e2) ] = decode [ Register_only; Any; Any ] in
      add_ir t (Instruction (Arith { op; rd; rs1; rs2 }));
      Or_null.iter e1 ~f:(fun e -> add_ir t (Data e));
      Or_null.iter e2 ~f:(fun e -> add_ir t (Data e))
    | `LDA ->
      let [ rd; (addr, addr_expr); offset ] =
        decode [ Register_only; Any; Optional Expr ]
      in
      add_ir t (Instruction (LDA { rd; addr; offset }));
      Or_null.iter addr_expr ~f:(fun e -> add_ir t (Data e))
    | `STO ->
      let [ (addr, e1); (rs, e2); offset ] = decode [ Any; Any; Optional Expr ] in
      add_ir t (Instruction (STO { addr; rs; offset }));
      Or_null.iter e1 ~f:(fun e -> add_ir t (Data e));
      Or_null.iter e2 ~f:(fun e -> add_ir t (Data e))
    | #Instruction.Op.branch as op ->
      let [ (rs, e1); offset ] = decode [ Any; Expr ] in
      let cur_pos = t.pos in
      let offset : IR.Expr.t =
        match offset with
        | Label l -> Sub (Label l, Constant cur_pos)
        | e -> e
      in
      add_ir t (Instruction (Branch { op; rs; offset }));
      Or_null.iter e1 ~f:(fun e -> add_ir t (Data e))
    | #Instruction.Op.jump as op ->
      let [ rd; (rs1, e1); (rs2, e2) ] = decode [ Register_only; Any; Optional Any ] in
      let cur_pos = t.pos in
      let e1 : IR.Expr.t or_null =
        (* handle making relative if needed *)
        match op, e1 with
        | `JAL, This (Label l) -> This (Sub (Label l, Constant cur_pos))
        | _, x -> x
      in
      add_ir t (Instruction (Jump { rd; op; rs1; rs2 }));
      Or_null.iter e1 ~f:(fun e -> add_ir t (Data e));
      Or_null.iter e2 ~f:(fun e -> add_ir t (Data e))
      (* Special: mostly just flipping around args *)
    | `nop ->
      let [] = decode [] in
      add_ir t (Instruction (Arith { op = `ADD; rd = `r0; rs1 = `r0; rs2 = `r0 }))
    | `sgu ->
      let [ rd; rs1; rs2 ] = decode [ Parsed_any; Parsed_any; Parsed_any ] in
      add_instruction t `SLU [ rd; rs2; rs1 ]
    | `sgt ->
      let [ rd; rs1; rs2 ] = decode [ Parsed_any; Parsed_any; Parsed_any ] in
      add_instruction t `SLT [ rd; rs2; rs1 ]
    (* negated comparisons: compute the opposite, then flip the bit *)
    | (`sle | `sge | `sleu | `sgeu) as op ->
      let [ rd; rs1; rs2 ] = decode [ Parsed_any; Parsed_any; Parsed_any ] in
      (match op with
       | `sle -> add_instruction t `SLT [ rd; rs2; rs1 ]
       | `sge -> add_instruction t `SLT [ rd; rs1; rs2 ]
       | `sleu -> add_instruction t `SLU [ rd; rs2; rs1 ]
       | `sgeu -> add_instruction t `SLU [ rd; rs1; rs2 ]);
      add_instruction t `XOR [ rd; rd; Constant 1 ]
    | `seq ->
      let [ rd; rs1; rs2 ] = decode [ Parsed_any; Parsed_any; Parsed_any ] in
      add_instruction t `XOR [ rd; rs1; rs2 ];
      add_instruction t `SLU [ rd; rd; Constant 1 ]
    | `sne ->
      let [ rd; rs1; rs2 ] = decode [ Parsed_any; Parsed_any; Parsed_any ] in
      add_instruction t `XOR [ rd; rs1; rs2 ];
      add_instruction t `SLU [ rd; Register (builtin_reg `r0); rd ]
    | `neg ->
      let [ rd; rs ] = decode [ Parsed_any; Parsed_any ] in
      add_instruction t `SUB [ rd; Register (builtin_reg `r0); rs ]
    | `not ->
      let [ rd; rs ] = decode [ Parsed_any; Parsed_any ] in
      add_instruction t `XOR [ rd; rs; Constant (-1) ]
    (* the opposite branch over a [j] *)
    | (`bnz | `blez) as op ->
      let [ rs; target ] = decode [ Parsed_any; Parsed_any ] in
      let branch_size =
        match rs with
        | Register _ -> 1
        | Constant _ | Label _ -> 2
      in
      let jump_size = 2 in
      let target : Asm_ast.Operand.t =
        (* a constant offset is relative to the branch, but the [j] comes after it *)
        match target with
        | Constant offset -> Constant (offset - branch_size)
        | Label _ -> target
        | Register _ -> failwith "expected expression"
      in
      add_instruction
        t
        (match op with
         | `bnz -> `BEZ
         | `blez -> `BGZ)
        [ rs; Constant (branch_size + jump_size) ];
      add_instruction t `j [ target ]
    | `mov ->
      let [ rd; rs ] = decode [ Parsed_any; Parsed_any ] in
      add_instruction t `ADD [ rd; rs; Register (builtin_reg `r0) ]
    | `jr -> add_instruction t `JLR (Register (builtin_reg `r0) :: operands)
    | `j -> add_instruction t `JAL (Register (builtin_reg `r0) :: operands)
  ;;

  let handle_statement t (statement : Asm_ast.Statement.t) =
    match statement with
    | Label_def (Global lbl) -> Hashtbl.add_exn t.global_labels ~key:lbl ~data:(Constant t.pos)
    | Label_def (Local lbl) ->
      add_ir t (Local_label lbl);
      let queue =
        Hashtbl.find_or_add t.local_labels lbl ~default:(fun () -> Queue.create ())
      in
      Queue.enqueue queue t.pos
    | Instruction { opcode; operands } ->
      add_instruction t (IR.Op.of_string (Asm_ast.Opcode.to_string opcode)) operands
  ;;
end

module Phase_2 = struct
  module Local_label_state = struct
    type t =
      { mutable cur : int or_null
      ; next : int Queue.t
      }

    let create next = { cur = Null; next }

    let resolve t (dir : Asm_ast.Label.Dir.t) =
      match dir with
      | Forwards -> Queue.peek_or_null t.next
      | Backwards -> t.cur
    ;;

    let advance t = t.cur <- Queue.dequeue_or_null t.next
  end

  type t =
    { output : U16.t array
    ; global_labels : IR.Expr.t Asm_ast.Label.Name.Table.t @@ global
    ; local_labels : Local_label_state.t Int.Table.t @@ global
    ; mutable idx : int
    }

  let create ~output ~global_labels ~local_labels = exclave_
    { output
    ; global_labels
    ; local_labels = Hashtbl.map local_labels ~f:Local_label_state.create
    ; idx = 0
    }
  ;;

  let add t x =
    t.output.(t.idx) <- U16.of_int_exn x;
    t.idx <- t.idx + 1
  ;;

  let rec evaluate (t @ local) ?(computing = Asm_ast.Label.Name.Set.empty) (expr : IR.Expr.t)
    =
    match expr with
    | Sub (e1, e2) -> evaluate t ~computing e1 - evaluate t ~computing e2
    | Constant n -> n
    | Label (Global name) ->
      if Set.mem computing name
      then raise_s [%message "Cyclic define" (name : Asm_ast.Label.Name.t)];
      evaluate
        t
        ~computing:(Set.add computing name)
        (Hashtbl.find_exn t.global_labels name)
    | Label (Local (n, dir)) ->
      (match Local_label_state.resolve (Hashtbl.find_exn t.local_labels n) dir with
       | This n -> n
       | Null | (exception _) ->
         raise_s [%message "Local label not found" (n : int) (dir : Asm_ast.Label.Dir.t)])
  ;;

  let handle_elem (t @ local) (elem : IR.Elem.t) =
    match elem with
    | Local_label n -> Local_label_state.advance (Hashtbl.find_exn t.local_labels n)
    | Data expr ->
      let n = evaluate t expr in
      if n < I16.min_int
      then failwithf "Value can't fit into 16 bits: %d" n ()
      else add t (if n < 0 then U16.max_int + 1 + n else n)
    | Instruction instr ->
      let encoded : Instruction.t =
        match instr with
        | Arith { op; rd; rs1; rs2 } -> Arith { op; rd; rs1; rs2 }
        | LDA { rd; addr; offset } ->
          LDA { rd; addr; offset = U4.of_int_exn (evaluate t offset) }
        | STO { addr; rs; offset } ->
          STO { addr; rs; offset = U4.of_int_exn (evaluate t offset) }
        | Branch { op; rs; offset } ->
          Branch { op; rs; offset = I7.of_int_exn (evaluate t offset) }
        | Jump { op; rd; rs1; rs2 } -> Jump { op; rd; rs1; rs2 }
      in
      add t (Instruction.to_int encoded)
  ;;
end

let assemble' statements =
  let phase1 = Phase_1.init () in
  List.iter statements ~f:(fun x -> Phase_1.handle_statement phase1 x);
  Iarray.create (U16.of_int_exn 0) ~len:phase1.pos ~mutate:(fun output ->
    let phase2 =
      Phase_2.create
        ~output
        ~global_labels:phase1.global_labels
        ~local_labels:phase1.local_labels
    in
    Queue.iter phase1.ir ~f:(fun elem -> Phase_2.handle_elem phase2 elem) [@nontail])
;;

let assemble input = assemble' (Asm_ast.parse input)
