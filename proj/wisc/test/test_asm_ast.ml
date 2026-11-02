open! Core

let%expect_test "Shape" =
  Wisc.Asm_ast.For_testing.print_shape ();
  [%expect
    {|
    ((⟦\t ⟧⋆
      ((: «⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ . «⟦\d⟧ ⟦\d⟧⋆»)
       ‖ «⟦A-Z_a-z⟧⋆ ⟦A-Z_a-z⟧»
         (⟦\t ⟧
          ⟦\t ⟧⋆
          ((«% ⟦0-9A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆»
            ‖ («⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ «⟦\d⟧ ⟦\d⟧⋆» (f ‖ b))
            ‖ «- ‖ ""»
              ((«⟦0-9⟧» ("" ‖ _))⋆ «⟦0-9⟧»
               ‖ 0o («⟦0-7⟧» ("" ‖ _))⋆ «⟦0-7⟧»
               ‖ 0b («⟦01⟧» ("" ‖ _))⋆ «⟦01⟧»
               ‖ 0x («⟦0-9A-Fa-f⟧» ("" ‖ _))⋆ «⟦0-9A-Fa-f⟧»))
           ⟦\t ⟧⋆
           ,
           ⟦\t ⟧⋆)⋆
          («% ⟦0-9A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆»
           ‖ («⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ «⟦\d⟧ ⟦\d⟧⋆» (f ‖ b))
           ‖ «- ‖ ""»
             ((«⟦0-9⟧» ("" ‖ _))⋆ «⟦0-9⟧»
              ‖ 0o («⟦0-7⟧» ("" ‖ _))⋆ «⟦0-7⟧»
              ‖ 0b («⟦01⟧» ("" ‖ _))⋆ «⟦01⟧»
              ‖ 0x («⟦0-9A-Fa-f⟧» ("" ‖ _))⋆ «⟦0-9A-Fa-f⟧»))
          ‖ ""))
      ‖ "")
     ⟦\t ⟧⋆
     (";" ⟦^\n\r⟧⋆ ‖ "")
     ("\r\n" ‖ "\n"))⋆
    (⟦\t ⟧⋆
     ((: «⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ . «⟦\d⟧ ⟦\d⟧⋆»)
      ‖ «⟦A-Z_a-z⟧⋆ ⟦A-Z_a-z⟧»
        (⟦\t ⟧
         ⟦\t ⟧⋆
         ((«% ⟦0-9A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆»
           ‖ («⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ «⟦\d⟧ ⟦\d⟧⋆» (f ‖ b))
           ‖ «- ‖ ""»
             ((«⟦0-9⟧» ("" ‖ _))⋆ «⟦0-9⟧»
              ‖ 0o («⟦0-7⟧» ("" ‖ _))⋆ «⟦0-7⟧»
              ‖ 0b («⟦01⟧» ("" ‖ _))⋆ «⟦01⟧»
              ‖ 0x («⟦0-9A-Fa-f⟧» ("" ‖ _))⋆ «⟦0-9A-Fa-f⟧»))
          ⟦\t ⟧⋆
          ,
          ⟦\t ⟧⋆)⋆
         («% ⟦0-9A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆»
          ‖ («⟦A-Z_a-z⟧ ⟦0-9A-Z_a-z⟧⋆» ‖ «⟦\d⟧ ⟦\d⟧⋆» (f ‖ b))
          ‖ «- ‖ ""»
            ((«⟦0-9⟧» ("" ‖ _))⋆ «⟦0-9⟧»
             ‖ 0o («⟦0-7⟧» ("" ‖ _))⋆ «⟦0-7⟧»
             ‖ 0b («⟦01⟧» ("" ‖ _))⋆ «⟦01⟧»
             ‖ 0x («⟦0-9A-Fa-f⟧» ("" ‖ _))⋆ «⟦0-9A-Fa-f⟧»))
         ‖ ""))
     ‖ "")
    ⟦\t ⟧⋆
    (";" ⟦^\n\r⟧⋆ ‖ "")
    |}]
;;

let%expect_test "Parser test" =
  let case s =
    Or_error.try_with (fun () -> Wisc.Asm_ast.parse s)
    |> [%sexp_of: Wisc.Asm_ast.Statement.t list Or_error.t]
    |> Expectree.sexp_to_string
    |> print_endline
  in
  case "";
  [%expect {| Ok╶╴• |}];
  case "dat";
  [%expect {|
    Ok
    ╰─╴Instruction
       ╰─╴opcode╶╴dat
    |}];
  case "instr 123, %hi, lbl, 4f";
  [%expect
    {|
    Ok
    ╰─╴Instruction
       ├─╴opcode╶╴instr
       ╰─╴operands
          ├─╴Constant╶╴123
          ├─╴Register╶╴%hi
          ├─╴Label
          │  ├─╴Global
          │  ╰─╴lbl
          ╰─╴Label
             ├─╴Local
             ├─╴4
             ╰─╴Forwards
    |}];
  case "instr %hi";
  [%expect
    {|
    Ok
    ╰─╴Instruction
       ├─╴opcode╶╴instr
       ╰─╴operands
          ╰─╴Register╶╴%hi
    |}];
  case
    {|
     :label
          sto %0, label, 3b
          dat 123  ; comment goes here!
          ; trailing comment
  |};
  [%expect
    {|
    Ok
    ├─╴Label_def
    │  ├─╴Global
    │  ╰─╴label
    ├─╴Instruction
    │  ├─╴opcode╶╴sto
    │  ╰─╴operands
    │     ├─╴Register╶╴%0
    │     ├─╴Label
    │     │  ├─╴Global
    │     │  ╰─╴label
    │     ╰─╴Label
    │        ├─╴Local
    │        ├─╴3
    │        ╰─╴Backwards
    ╰─╴Instruction
       ├─╴opcode╶╴dat
       ╰─╴operands
          ╰─╴Constant╶╴123
    |}];
  ()
;;
