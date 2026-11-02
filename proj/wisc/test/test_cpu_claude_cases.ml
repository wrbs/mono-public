open! Core
open Wisc.Ints

let%expect_test "fib" =
  let source =
    {|
alias %i,  %d

  mov %a, 1
  mov %b, 1
  mov %i, 10
.1
  bez %i, 2f
  mov %c, %a
  add %a, %a, %b
  mov %b, %c
  sub %i, %i, 1
  j 1b

.2
  sto 0xFFFF, 1
|}
  in
  let prog = Wisc.Asm.assemble source in
  let ram =
    Array.create ~len:U16.max_int (* not plus 1! last = io *) (U16.of_int_exn 0)
  in
  Iarray.iteri prog ~f:(fun idx n -> ram.(idx) <- n);
  let io_addr = U16.of_int_exn 0xFFFF in
  let (Simulator (state, sim)) = Wisc.Simulator.create ~init_pc:(U16.of_int_exn 0) in
  let rec aux (T { current_io; step } : Wisc.Simulator.t) ~steps_left =
    if steps_left <= 0 then failwith "used too many steps!";
    let next input = aux (step input) ~steps_left:(steps_left - 1) in
    match current_io with
    | Write (addr, _) when [%equal: U16.t] addr io_addr -> ()
    | Read addr when [%equal: U16.t] addr io_addr -> failwith "Read from io"
    | Write (addr, data) ->
      ram.(U16.to_int addr) <- data;
      next ()
    | Read addr -> next ram.(U16.to_int addr)
    | Nope -> next ()
  in
  aux sim ~steps_left:1024;
  Expectree.sexp_to_string [%sexp (state : Wisc.Simulator.Current_state.t)]
  |> print_endline;
  [%expect {|
    ╭─╴pc╶╴000e
    ├─╴r1╶╴0000[0]
    ├─╴r2╶╴0000[0]
    ├─╴r3╶╴0000[0]
    ├─╴r4╶╴0000[0]
    ├─╴r5╶╴0000[0]
    ├─╴r6╶╴0000[0]
    ├─╴r7╶╴0000[0]
    ├─╴r8╶╴0000[0]
    ├─╴r9╶╴0000[0]
    ├─╴ra╶╴0090[144]
    ├─╴rb╶╴0059[89]
    ├─╴rc╶╴0059[89]
    ├─╴rd╶╴0000[0]
    ╰─╴re╶╴0000[0]
    |}]
;;

(* IO convention for the tests below, [define]d by [prelude]: [sto out, x] prints [x] and
   [sto halt, x] stops the simulation. *)
let prelude =
  {|
define out, 0xFFFF
define halt, 0xFFFE
|}
;;

let out_addr = U16.of_int_exn 0xFFFF
let halt_addr = U16.of_int_exn 0xFFFE

(* [origin] is where the program is loaded and starts; the assembler always assumes 0. *)
let run ?(max_cycles = 100_000) ?(origin = 0) source =
  match Wisc.Asm.assemble (prelude ^ source) with
  | exception exn -> print_s [%message "Assembly failed" (exn : exn)]
  | prog ->
    let ram = Array.create ~len:(U16.max_int + 1) (U16.of_int_exn 0) in
    Iarray.iteri prog ~f:(fun idx n -> ram.(origin + idx) <- n);
    let (Simulator (_, sim)) = Wisc.Simulator.create ~init_pc:(U16.of_int_exn origin) in
    let rec aux (T { current_io; step } : Wisc.Simulator.t) ~cycles_left =
      if cycles_left <= 0
      then print_endline "did not halt"
      else (
        let next input = aux (step input) ~cycles_left:(cycles_left - 1) in
        match current_io with
        | Write (addr, _) when [%equal: U16.t] addr halt_addr -> ()
        | Write (addr, data) when [%equal: U16.t] addr out_addr ->
          printf "%s[%d]\n" (U16.Hex.to_string data) (I16.to_int (I16.of_u16 data));
          next ()
        | Read addr when U16.to_int addr >= U16.to_int halt_addr ->
          raise_s [%message "Read from io" (addr : U16.Hex.t)]
        | Write (addr, data) ->
          ram.(U16.to_int addr) <- data;
          next ()
        | Read addr -> next ram.(U16.to_int addr)
        | Nope -> next ())
    in
    aux sim ~cycles_left:max_cycles
;;

let%expect_test "arithmetic" =
  run
    {|
  mov %1, 100
  mov %2, 7
  add %3, %1, %2
  sto out, %3
  sub %3, %1, %2
  sto out, %3
  sub %3, %2, %1
  sto out, %3
  mul %3, %1, %2
  sto out, %3
  mul %3, 0x100, 0x100  ; overflow wraps
  sto out, %3
  mov %3, -5
  sto out, %3
  sto out, 42           ; immediates in both operands
  sto halt, 0
|};
  [%expect {|
    006b[107]
    005d[93]
    ffa3[-93]
    02bc[700]
    0000[0]
    fffb[-5]
    002a[42]
    |}]
;;

let%expect_test "bitwise and shifts" =
  run
    {|
  and %1, 0b1100, 0b1010
  sto out, %1
  bor %1, 0b1100, 0b1010
  sto out, %1
  xor %1, 0b1100, 0b1010
  sto out, %1
  shl %1, 1, 15
  sto out, %1
  shl %1, 1, 16         ; only the bottom 4 bits of the shift amount count
  sto out, %1
  shr %1, 0x8000, 15
  sto out, %1
  asr %1, 0x8000, 15
  sto out, %1
  slu %1, 1, 2
  sto out, %1
  slu %1, 2, 1
  sto out, %1
  slu %1, -1, 1         ; unsigned
  sto out, %1
  sgu %1, 2, 1
  sto out, %1
  slt %1, -1, 1
  sto out, %1
  slt %1, 1, -1
  sto out, %1
  slt %1, 0x7FFF, 0x8000  ; 32767 < -32768?
  sto out, %1
  slt %1, 0x8000, 0x7FFF
  sto out, %1
  slt %1, -3, -3
  sto out, %1
  sto halt, 0
|};
  [%expect {|
    0008[8]
    000e[14]
    0006[6]
    8000[-32768]
    0001[1]
    0001[1]
    ffff[-1]
    0001[1]
    0000[0]
    0000[0]
    0001[1]
    0001[1]
    0000[0]
    0000[0]
    0001[1]
    0000[0]
    |}]
;;

let%expect_test "arithmetic edge cases" =
  run
    {|
  add %1, 0xFFFF, 1
  sto out, %1
  sub %1, 0, 1
  sto out, %1
  mul %1, -3, 7
  sto out, %1
  mul %1, 0x7FFF, 2
  sto out, %1
  asr %1, 0x4000, 14
  sto out, %1
  asr %1, -16, 2
  sto out, %1
  shr %1, 0xFFFF, 0
  sto out, %1
  mov %1, 0x1234
  xor %1, %1, %1
  sto out, %1
  and %1, -1, 0x1234
  sto out, %1
  slu %1, 5, 5
  sto out, %1
  sgu %1, -1, 1
  sto out, %1
  sto halt, 0
|};
  [%expect {|
    0000[0]
    ffff[-1]
    ffeb[-21]
    fffe[-2]
    0001[1]
    fffc[-4]
    ffff[-1]
    0000[0]
    1234[4660]
    0000[0]
    0001[1]
    |}]
;;

let%expect_test "every register" =
  run
    {|
  mov %1, 1
  mov %2, 2
  mov %3, 3
  mov %4, 4
  mov %5, 5
  mov %6, 6
  mov %7, 7
  mov %8, 8
  mov %9, 9
  mov %a, 10
  mov %b, 11
  mov %c, 12
  mov %d, 13
  mov %e, 14
  sto out, %1
  sto out, %2
  sto out, %3
  sto out, %4
  sto out, %5
  sto out, %6
  sto out, %7
  sto out, %8
  sto out, %9
  sto out, %a
  sto out, %b
  sto out, %c
  sto out, %d
  sto out, %e
  sto halt, 0
|};
  [%expect {|
    0001[1]
    0002[2]
    0003[3]
    0004[4]
    0005[5]
    0006[6]
    0007[7]
    0008[8]
    0009[9]
    000a[10]
    000b[11]
    000c[12]
    000d[13]
    000e[14]
    |}]
;;

let%expect_test "immediate in each operand position" =
  run
    {|
  mov %2, 10
  sub %1, %2, 3
  sto out, %1
  sub %1, 3, %2
  sto out, %1
  sub %1, 30, 3
  sto out, %1
  sub %1, %2, %2
  sto out, %1
  mov %3, 0x2000
  sto %3, 77
  sto 0x2000, %2, 1
  lda %1, %3
  sto out, %1
  lda %1, 0x2000, 1
  sto out, %1
  sto halt, 0
|};
  [%expect {|
    0007[7]
    fff9[-7]
    001b[27]
    0000[0]
    004d[77]
    000a[10]
    |}]
;;

let%expect_test "r0 and rf" =
  run
    {|
  add %0, 1, 2          ; writes to r0 are ignored
  sto out, %0
  add %f, 1, 2          ; as are writes to rf
  sto out, 1
  nop
  sto halt, 0
|};
  [%expect {|
    0000[0]
    0001[1]
    |}]
;;

let%expect_test "pseudo-instructions" =
  run
    {|
  sgt %1, 2, 1
  sto out, %1
  sgt %1, -1, 1
  sto out, %1
  sle %1, 1, 1
  sto out, %1
  sle %1, 2, 1
  sto out, %1
  sle %1, -1, 1
  sto out, %1
  sge %1, 1, 1
  sto out, %1
  sge %1, -1, 1
  sto out, %1
  sleu %1, 1, -1
  sto out, %1
  sleu %1, -1, 1
  sto out, %1
  sgeu %1, -1, 1
  sto out, %1
  seq %1, 5, 5
  sto out, %1
  seq %1, 5, 6
  sto out, %1
  mov %1, 5
  seq %1, %1, 5         ; rd aliasing an operand
  sto out, %1
  sne %1, 5, 5
  sto out, %1
  sne %1, -5, 5
  sto out, %1
  neg %1, 5
  sto out, %1
  not %1, 0x00FF
  sto out, %1
  mov %2, -3
  bnz %2, 1f
  sto out, 666          ; skipped
.1
  bnz 0, 1f
  sto out, 1
.1
  blez %2, 1f
  sto out, 667          ; skipped
.1
  blez 5, 1f
  sto out, 2
.1
  mov %3, -3
.2
  sto out, %3
  add %3, %3, 1
  bnz %3, 2b
  sto halt, 0
|};
  [%expect {|
    0001[1]
    0000[0]
    0001[1]
    0000[0]
    0001[1]
    0001[1]
    0000[0]
    0001[1]
    0000[0]
    0001[1]
    0001[1]
    0000[0]
    0001[1]
    0000[0]
    0001[1]
    fffb[-5]
    ff00[-256]
    0001[1]
    0002[2]
    fffd[-3]
    fffe[-2]
    ffff[-1]
    |}]
;;

let%expect_test "memory" =
  run
    {|
  lda %1, table
  sto out, %1
  lda %1, table, 2
  sto out, %1
  mov %2, table
  lda %1, %2, 1
  sto out, %1
  sto %2, 99, 1
  lda %1, table, 1
  sto out, %1
  sto 0x1000, 1234, 15
  lda %1, 0x1000, 15
  sto out, %1
  sto halt, 0
:table
  dat 10, 20, 30
|};
  [%expect {|
    000a[10]
    001e[30]
    0014[20]
    0063[99]
    04d2[1234]
    |}]
;;

let%expect_test "memory edge cases" =
  run
    {|
  mov %2, 0xFFFF
  sto %2, 7, 1          ; address arithmetic wraps: writes to 0
  lda %1, 0
  sto out, %1
  lda %0, 0             ; loads into r0/rf are discarded
  lda %f, 0
  sto out, %0
  sto halt, 0
|};
  [%expect {|
    0007[7]
    0000[0]
    |}]
;;

let%expect_test "self-modifying code" =
  run
    {|
  sto target, 99, 1     ; overwrite the immediate of [mov]
:target
  mov %1, 1
  sto out, %1
  sto halt, 0
|};
  [%expect {| 0063[99] |}]
;;

let%expect_test "branches" =
  run
    {|
  mov %1, 0
  bez %1, 1f
  sto out, 1            ; skipped
.1
  sto out, 2
  mov %1, -1
  bgz %1, 1f            ; signed: -1 is not > 0
  sto out, 3
.1
  mov %1, 5
  bgz %1, 1f
  sto out, 4            ; skipped
.1
  bez 0, 1f             ; immediate operand
  sto out, 5            ; skipped
.1
  mov %2, 3
.2
  sto out, %2
  sub %2, %2, 1
  bgz %2, 2b
  sto halt, 0
|};
  [%expect {|
    0002[2]
    0003[3]
    0003[3]
    0002[2]
    0001[1]
    |}]
;;

let%expect_test "branch edge cases" =
  run
    {|
  bgz 0, 1f             ; 0 is not > 0
  sto out, 1
.1
  bgz 0x7FFF, 1f
  sto out, 2            ; skipped
.1
  bgz 0x8000, 1f        ; negative
  sto out, 3
.1
  bez 1, 1f
  sto out, 4
.1
  sto halt, 0
|};
  [%expect {|
    0001[1]
    0003[3]
    0004[4]
    |}]
;;

let%expect_test "branch range" =
  let dats n = String.concat (List.init n ~f:(fun _ -> "  dat 0\n")) in
  let forwards n =
    run [%string "  bez 0, 1f\n%{dats n}.1\n  sto out, 1\n  sto halt, 0"]
  in
  let backwards n =
    run
      [%string
        "  j 2f\n.1\n  sto out, 1\n  sto halt, 0\n%{dats n}.2\n  bez 0, 1b"]
  in
  (* the largest offsets in each direction: +63 and -64 *)
  forwards 61;
  [%expect {| 0001[1] |}];
  backwards 58;
  [%expect {| 0001[1] |}]
;;

let%expect_test "nested local labels" =
  run
    {|
  mov %1, 2
.1
  mov %2, 2
.2
  mul %3, %1, 10
  add %3, %3, %2
  sto out, %3
  sub %2, %2, 1
  bgz %2, 2b
  sub %1, %1, 1
  bgz %1, 1b
  sto halt, 0
|};
  [%expect {|
    0016[22]
    0015[21]
    000c[12]
    000b[11]
    |}]
;;

let%expect_test "jumps and calls" =
  run
    {|
  mov %1, 5
  jal %e, double
  sto out, %1
  jal %e, double
  sto out, %1
  j 1f
  sto out, 666          ; skipped
.1
  mov %2, done
  jr %2
  sto out, 667          ; skipped
:done
  sto halt, 0

:double
  add %1, %1, %1
  jr %e
|};
  [%expect {|
    000a[10]
    0014[20]
    |}]
;;

let%expect_test "return addresses" =
  run
    {|
  jal %e, 1f            ; at address 0, two words long
.1
  sto out, %e
  jal %0, 1f            ; link discarded
.1
  sto out, %0
  mov %2, 2f
  jlr %e, %2            ; absolute jump, linking
.1
  sto out, %e
  sto halt, 0
.2
  jr %e, 0
|};
  [%expect {|
    0002[2]
    0000[0]
    000b[11]
    |}]
;;

let%expect_test "jump table" =
  run
    {|
  mov %1, 0
.1
  add %2, %1, %1        ; each [j] is two words
  jr %2, table
.2
  add %1, %1, 1
  slu %3, %1, 3
  bgz %3, 1b
  sto halt, 0
:table
  j case0
  j case1
  j case2
:case0
  sto out, 100
  j 2b
:case1
  sto out, 101
  j 2b
:case2
  sto out, 102
  j 2b
|};
  [%expect {|
    0064[100]
    0065[101]
    0066[102]
    |}]
;;

let%expect_test "alias and define" =
  run
    {|
alias %acc, %1
define three, 3
define also_three, three  ; refers to an earlier define
define four, later        ; refers to a later define
define later, 4
define end_ref, end       ; refers to a label
  mov %acc, also_three
  sto out, %acc
  sto out, four
  mov %2, end_ref
  sub %2, %2, end
  sto out, %2
  j end_ref
  sto out, 666          ; skipped
:end
  sto halt, 0
|};
  [%expect {|
    0003[3]
    0004[4]
    0000[0]
    |}]
;;

let%expect_test "define in other positions" =
  run
    {|
define len, 3
define off, 2
define table_end, end
  mov %2, table
  lda %1, %2, off
  sto out, %1
  lda %1, table, len
  sto out, %1
  sub %1, table_end, table
  sto out, %1
  sto halt, 0
:table
  dat len, 30, table, 40
:end
|};
  [%expect {|
    0011[17]
    0028[40]
    0004[4]
    |}]
;;

let%expect_test "gcd" =
  run
    {|
  mov %1, 1071
  mov %2, 462
.1
  sub %3, %1, %2
  bez %3, 3f
  slu %4, %1, %2
  bez %4, 2f
  sub %2, %2, %1
  j 1b
.2
  mov %1, %3
  j 1b
.3
  sto out, %1
  sto halt, 0
|};
  [%expect {| 0015[21] |}]
;;

let%expect_test "sum and copy array" =
  run
    {|
  mov %1, src
  mov %2, dst
  mov %3, 5
  mov %4, 0
.1
  bez %3, 2f
  lda %5, %1
  add %4, %4, %5
  sto %2, %5
  add %1, %1, 1
  add %2, %2, 1
  sub %3, %3, 1
  j 1b
.2
  sto out, %4
  lda %5, dst
  sto out, %5
  lda %5, dst, 4
  sto out, %5
  sto halt, 0
:src
  dat 1, 2, 3, 4, -5
:dst
  dat 0, 0, 0, 0, 0
|};
  [%expect {|
    0005[5]
    0001[1]
    fffb[-5]
    |}]
;;

let%expect_test "shift-and-add multiply" =
  run
    {|
  mov %1, 123
  mov %2, 45
  mov %3, 0
.1
  bez %2, 2f
  and %4, %2, 1
  bez %4, 3f
  add %3, %3, %1
.3
  shl %1, %1, 1
  shr %2, %2, 1
  j 1b
.2
  sto out, %3
  mul %4, 123, 45
  sto out, %4
  sto halt, 0
|};
  [%expect {|
    159f[5535]
    159f[5535]
    |}]
;;

let%expect_test "popcount" =
  run
    {|
  mov %1, 0xB6F5
  mov %2, 0
.1
  bez %1, 2f
  and %3, %1, 1
  add %2, %2, %3
  shr %1, %1, 1
  j 1b
.2
  sto out, %2
  sto halt, 0
|};
  [%expect {| 000b[11] |}]
;;

let%expect_test "recursive fib with a stack" =
  run
    {|
alias %sp, %d
alias %ra, %e
  mov %sp, 0x8000
  mov %1, 10
  jal %ra, fib
  sto out, %2
  sto out, %sp          ; balanced
  sto halt, 0

; %2 = fib(%1), clobbers %1 and %3
:fib
  slu %3, %1, 2
  bez %3, 1f
  mov %2, %1
  jr %ra
.1
  sub %sp, %sp, 3
  sto %sp, %ra, 0
  sto %sp, %1, 1
  sub %1, %1, 1
  jal %ra, fib
  sto %sp, %2, 2
  lda %1, %sp, 1
  sub %1, %1, 2
  jal %ra, fib
  lda %3, %sp, 2
  add %2, %2, %3
  lda %ra, %sp, 0
  add %sp, %sp, 3
  jr %ra
|};
  [%expect {|
    0037[55]
    8000[-32768]
    |}]
;;

(* Calls, branches and [j] are pc-relative already; data and computed jumps need the
   program's runtime address, which [jal] gives us. The assembler only knows absolute label
   values, but the difference of two is position-independent. *)
let%expect_test "position-independent code" =
  let program =
    {|
  jal %4, 1f            ; %4 = runtime address of .1
.1
  add %5, %4, table     ; %5 = %4 + (table - .1)
  sub %5, %5, 1b
  lda %1, %5, 1
  sto out, %1
  jal %e, double
  sto out, %1
  add %6, %4, cases
  sub %6, %6, 1b
  jr %6, 2              ; second entry
.2
  sto out, %1
  lda %1, table         ; for contrast: absolute, only right at origin 0
  sto out, %1
  sto halt, 0

:double
  add %1, %1, %1
  jr %e

:cases
  j case0
  j case1
:case0
  mov %1, 100
  j 2b
:case1
  mov %1, 101
  j 2b

:table
  dat 10, 20, 30
|}
  in
  List.iter [ 0; 0x3000; 0xabc ] ~f:(fun origin ->
    printf "origin %#x\n" origin;
    run ~origin program);
  [%expect {|
    origin 0
    0014[20]
    0028[40]
    0065[101]
    000a[10]
    origin 0x3000
    0014[20]
    0028[40]
    0065[101]
    0000[0]
    origin 0xabc
    0014[20]
    0028[40]
    0065[101]
    0000[0]
    |}]
;;

let%expect_test "stack frames" =
  run
    {|
; args in %1, %2; result in %1; %1-%6 caller-saved
; %sp grows down and points at the last used word
; non-leaf frame: fp+0 saved fp, fp+1 saved ra, fp+2.. locals
alias %fp, %c
alias %sp, %d
alias %ra, %e
  mov %sp, 0x8000
  mov %fp, 0
  mov %1, 3
  mov %2, 4
  jal %ra, sumsq
  sto out, %1
  mov %1, 6
  jal %ra, fact
  sto out, %1
  sto out, %sp          ; restored
  sto out, %fp
  sto halt, 0

; leaf, so no frame
:square
  mul %1, %1, %1
  jr %ra

; square(a) + square(b); locals: fp+2 b, fp+3 square(a)
define sumsq_frame, 4
:sumsq
  sub %sp, %sp, sumsq_frame
  sto %sp, %fp, 0
  sto %sp, %ra, 1
  mov %fp, %sp
  sto %fp, %2, 2
  jal %ra, square
  sto %fp, %1, 3
  lda %1, %fp, 2
  jal %ra, square
  lda %2, %fp, 3
  add %1, %1, %2
  mov %sp, %fp
  lda %ra, %sp, 1
  lda %fp, %sp, 0
  add %sp, %sp, sumsq_frame
  jr %ra

; n * fact(n - 1); locals: fp+2 n
define fact_frame, 3
:fact
  bgz %1, 1f
  mov %1, 1
  jr %ra
.1
  sub %sp, %sp, fact_frame
  sto %sp, %fp, 0
  sto %sp, %ra, 1
  mov %fp, %sp
  sto %fp, %1, 2
  sub %1, %1, 1
  jal %ra, fact
  lda %2, %fp, 2
  mul %1, %1, %2
  mov %sp, %fp
  lda %ra, %sp, 1
  lda %fp, %sp, 0
  add %sp, %sp, fact_frame
  jr %ra
|};
  [%expect {|
    0019[25]
    02d0[720]
    8000[-32768]
    0000[0]
    |}]
;;

let%expect_test "assembly errors" =
  run {|
define a, b
define b, a
  mov %1, a
|};
  [%expect {| ("Assembly failed" (exn ("Cyclic define" (name a)))) |}];
  run {|
define a, b
define b, a             ; unused, so never expanded
  sto halt, 0
|};
  [%expect {| |}];
  run "  j nowhere";
  [%expect {|
    ("Assembly failed"
     (exn (Not_found_s ("Hashtbl.find_exn: not found" nowhere))))
    |}];
  run ":x\n:x";
  [%expect {| ("Assembly failed" (exn ("Hashtbl.add_exn got key already present" x))) |}];
  run "define out, 3";
  [%expect {| ("Assembly failed" (exn ("Hashtbl.add_exn got key already present" out))) |}];
  run "define x, 1b";
  [%expect {| ("Assembly failed" (exn (Failure "local labels can't be used in a define"))) |}];
  run "  add %1, %2";
  [%expect {| ("Assembly failed" (exn (Failure "Too few operands"))) |}];
  run "  nop 1";
  [%expect {| ("Assembly failed" (exn (Failure "too many operands"))) |}];
  run "  bogus %1";
  [%expect {|
    ("Assembly failed"
     (exn ("Asm.IR.Op.of_string: invalid string" (value bogus))))
    |}];
  run "  mov %1, %nope";
  [%expect {| ("Assembly failed" (exn (Not_found_s ("Hashtbl.find_exn: not found" %nope)))) |}];
  run "  mov 1, 2";
  [%expect {| ("Assembly failed" (exn (Failure "expected register"))) |}];
  run "  mov %1, 0x10000";
  [%expect {|
    ("Assembly failed"
     (exn (Failure "U16.of_int_exn: value out of range 65536")))
    |}]
;;

let%expect_test "running off the end" =
  run ~max_cycles:100 "  mov %1, 1";
  [%expect {| did not halt |}]
;;
