open! Core
open! Hardcaml
module Cpu = Hardnes_rp2a03.Cpu

module U8 = struct
  type t = Int_repr.Uint8.t [@@deriving equal]

  let zero = Int_repr.Uint8.zero
  let of_int n : t = Int_repr.Uint8.of_base_int_exn n
  let of_char c = of_int (Char.to_int c)
  let of_hex s = Scanf.sscanf s "%x" of_int

  let of_bits b =
    [%test_eq: int] 8 (Bits.width b);
    of_int (Bits.to_unsigned_int b)
  ;;

  let to_int t = Int_repr.Uint8.to_base_int t
  let to_char t = Char.of_int_exn (to_int t)
  let to_bits t = Bits.of_unsigned_int ~width:8 (to_int t)
  let to_hex t = Printf.sprintf "%02X" (to_int t)
  let sim_get port = !port |> of_bits
  let sim_set port t = port := to_bits t
  let is_bit_set t bit = (to_int t lsr bit) land 1 = 1
end

module U16 = struct
  type t = Int_repr.Uint16.t [@@deriving equal]

  let of_int n : t = Int_repr.Uint16.of_base_int_exn n
  let of_hex s = Scanf.sscanf s "%x" of_int

  let of_bits b =
    [%test_eq: int] 16 (Bits.width b);
    of_int (Bits.to_unsigned_int b)
  ;;

  let to_int t = Int_repr.Uint16.to_base_int t
  let to_hex t = Printf.sprintf "%04X" (to_int t)
  let hi_char t = Char.of_int_exn (to_int t lsr 8)
  let sim_get port = !port |> of_bits
end

module Trace_state = struct
  type t =
    { pc : U16.t
    ; a : U8.t
    ; x : U8.t
    ; y : U8.t
    ; p : U8.t
    ; s : U8.t
    ; cycles : int
    }
  [@@deriving equal, fields ~getters]
end

module Trace_line = struct
  type t =
    { line_num : int
    ; instr_bytes : string
    ; dis : string
    ; state : Trace_state.t
    }
  [@@deriving fields ~getters]

  let re_t =
    let open Re in
    let hex_digit = alt [ rg '0' '9'; rg 'a' 'f'; rg 'A' 'F' ] in
    let hex n = repn hex_digit n None in
    let sep1 re ~sep = seq [ re; rep (seq [ sep; re ]) ] in
    let field name re = seq [ char ' '; str name; char ':'; group re ] in
    let reg name = field name (hex 2) in
    seq
      [ bol
      ; group (hex 4)
      ; str "  "
      ; group (sep1 (hex 2) ~sep:(char ' '))
      ; rep (char ' ')
      ; group (seq [ alt [ char ' '; char '*' ]; non_greedy (rep any) ])
      ; rep (char ' ')
      ; reg "A"
      ; reg "X"
      ; reg "Y"
      ; reg "P"
      ; reg "SP"
      ; rep any (* todo: scanline for ppu timing *)
      ; field "CYC" (rep1 digit)
      ; eol
      ]
  ;;

  let of_match group ~line_num =
    let g = Re.Group.all group in
    let pc = U16.of_hex g.(1) in
    let instr_bytes = g.(2) in
    let dis = g.(3) in
    let a = U8.of_hex g.(4) in
    let x = U8.of_hex g.(5) in
    let y = U8.of_hex g.(6) in
    let p = U8.of_hex g.(7) in
    let s = U8.of_hex g.(8) in
    let cycles = Int.of_string g.(9) in
    { line_num; instr_bytes; dis; state = { pc; a; x; y; p; s; cycles } }
  ;;

  let parse file =
    let re = Re.compile re_t in
    String.split_lines file
    |> List.mapi ~f:(fun idx line -> Re.exec re line |> of_match ~line_num:(idx + 1))
  ;;

  let load () = In_channel.read_all "nestest.log" |> parse

  module Print_part = struct
    type nonrec line = t
    type 'a format = int * ('a -> string)

    type t =
      | Fixed : string -> t
      | Line : ((line -> 'a) * 'a format) -> t
      | State : ((Trace_state.t -> 'a) * 'a format) -> t

    let show_flags f =
      let bit c n = if U8.is_bit_set f n then c else '.' in
      String.of_char_list
        [ bit 'N' 7; bit 'V' 6; bit 'D' 3; bit 'I' 2; bit 'Z' 1; bit 'C' 0 ]
    ;;

    let parts =
      let fixed s = [ Fixed s ] in
      let line get format = [ Line (get, format) ] in
      let state get format = [ State (get, format) ] in
      let str width = width, Fn.id in
      let u16 = 4, U16.to_hex in
      let u8 = 2, U8.to_hex in
      let field name get format = [ Fixed (" " ^ name ^ ":"); State (get, format) ] in
      let reg name get = field name get u8 in
      let open Trace_state in
      List.concat
        [ line line_num (4, fun n -> Int.to_string n)
        ; fixed "  "
        ; state pc u16
        ; fixed "  "
        ; line instr_bytes (str 10)
        ; line dis (str 32)
        ; reg "A" a
        ; reg "X" x
        ; reg "Y" y
        ; reg "P" p
        ; reg "S" s
        ; fixed " "
        ; state p (6, show_flags)
        ; field "CYC" cycles (10, fun n -> Int.to_string_hum n)
        ]
    ;;

    let total_width =
      List.sum (module Int) parts ~f:(function
        | Fixed s -> String.length s
        | Line (_, (len, _)) | State (_, (len, _)) -> len)
    ;;
  end

  let ~to_string, ~diff_line =
    let buffer = Buffer.create Print_part.total_width in
    let spaces n =
      for _ = 1 to n do
        Buffer.add_char buffer ' '
      done
    in
    let add_width width s =
      let n = String.length s in
      if n > width then failwithf "Field too long for width=%d (is %d): '%s'" n n s ();
      Buffer.add_string buffer s;
      spaces (width - n)
    in
    let to_string line =
      Buffer.clear buffer;
      let print_format (width, to_string) x =
        let s = to_string x in
        add_width width s
      in
      List.iter Print_part.parts ~f:(function
        | Fixed s -> Buffer.add_string buffer s
        | Line (get, format) -> print_format format (get line)
        | State (get, format) -> print_format format (get line.state));
      Buffer.contents buffer
    in
    let diff_line ~expected ~actual =
      Buffer.clear buffer;
      List.iter Print_part.parts ~f:(function
        | Fixed s -> spaces (String.length s)
        | Line (_, (width, _)) -> spaces width
        | State (get, (width, to_string)) ->
          let a = to_string (get expected.state) in
          let b = to_string (get actual) in
          (match [%equal: string] a b with
           | true -> spaces width
           | false -> add_width width b));
      Buffer.contents buffer
    in
    ~to_string, ~diff_line
  ;;
end

module type IO = sig
  type t

  val read : t -> addr:Int_repr.Uint16.t -> Int_repr.Uint8.t
  val write : t -> addr:Int_repr.Uint16.t -> data:Int_repr.Uint8.t -> unit
end

let get_bool port = Bits.to_bool !port

module Make_sim (I : IO) = struct
  type t =
    { sim : (Bits.t ref Cpu.I.t, Bits.t ref Cpu.O.t) Cyclesim.t
    ; mutable cycles : int
    ; io : I.t
    ; waves : Hardcaml_waveterm.Waveform.t option
    }

  let create io ~enable_waves =
    let module S = Cyclesim.With_interface (Cpu.I) (Cpu.O) in
    let sim = S.create (Cpu.create (Scope.create ~flatten_design:true ())) in
    let waves, sim =
      match enable_waves with
      | false -> None, sim
      | true ->
        let waves, sim = Hardcaml_waveterm.Waveform.create sim in
        Some waves, sim
    in
    (Cyclesim.inputs sim).enable := Bits.vdd;
    { sim; cycles = 0; io; waves }
  ;;

  let cycle t =
    let i = Cyclesim.inputs t.sim in
    let o = Cyclesim.outputs t.sim in
    let addr = U16.sim_get o.mem.addr in
    let data =
      match get_bool o.mem.write with
      | false -> I.read t.io ~addr
      | true ->
        let data = U8.sim_get o.mem.data in
        I.write t.io ~addr ~data;
        data
    in
    U8.sim_set i.data data;
    t.cycles <- t.cycles + 1;
    Cyclesim.cycle t.sim
  ;;

  let step t =
    let o = Cyclesim.outputs t.sim in
    let limit = 10 in
    let rec loop ~step =
      if step = limit then failwith "took too many steps!";
      cycle t;
      match get_bool o.fetching with
      | true -> ()
      | false -> loop ~step:(step + 1)
    in
    loop ~step:0
  ;;

  let trace t : Trace_state.t =
    let o = Cyclesim.outputs t.sim in
    { pc = U16.sim_get o.pc
    ; a = U8.sim_get o.a
    ; x = U8.sim_get o.x
    ; y = U8.sim_get o.y
    ; s = U8.sim_get o.s
    ; p = U8.sim_get o.p
    ; cycles = t.cycles
    }
  ;;
end

module Sim = struct
  module IO = struct
    type t =
      { prg_rom : string
      ; ram : Bytes.Hexdump.t
      }

    let read t ~addr =
      match U16.hi_char addr with
      | '\x00' .. '\x1f' ->
        let ram_addr = U16.to_int addr % 0x800 in
        Bytes.get t.ram ram_addr |> U8.of_char
      | '\x80' .. '\xff' ->
        let rom_addr = U16.to_int addr % String.length t.prg_rom in
        String.nget t.prg_rom rom_addr |> U8.of_char
      | _ -> U8.zero
    ;;

    let write t ~addr ~data =
      match U16.hi_char addr with
      | '\x00' .. '\x1f' ->
        let ram_addr = U16.to_int addr % 0x800 in
        Bytes.set t.ram ram_addr (U8.to_char data)
      | _ -> ()
    ;;

    let create (rom : Nes_file.t) =
      let prg_rom = Bigstring.to_string rom.prg_rom in
      let ram = Bytes.make 0x800 '\x00' in
      { prg_rom; ram }
    ;;
  end

  include Make_sim (IO)

  let create rom ~enable_waves = create (IO.create rom) ~enable_waves
end

module Lookback = struct
  type 'a t =
    { mutable left : int
    ; queue : 'a Queue.t
    }

  let create capacity = { left = capacity; queue = Queue.create ~capacity () }

  let push t elem =
    match t.left with
    | 0 ->
      Queue.dequeue_and_ignore_exn t.queue;
      Queue.enqueue t.queue elem
    | n ->
      t.left <- n - 1;
      Queue.enqueue t.queue elem
  ;;

  let iter t ~f = Queue.iter t.queue ~f
end

let change_reset_vector (rom : Nes_file.t) =
  let len = Bigstring.length rom.prg_rom in
  let set a n = Bigstring.set rom.prg_rom (len - (0x10000 - a)) (Char.of_int_exn n) in
  set 0xFFFC 0x00;
  set 0xFFFD 0xC0
;;

let run_sim ~enable_waves =
  let lines = Trace_line.load () in
  let rom = In_channel.read_all "nestest.nes" |> Nes_file.of_string |> Or_error.ok_exn in
  change_reset_vector rom;
  let sim = Sim.create rom ~enable_waves in
  Sim.step sim;
  let lookback = Lookback.create 10 in
  let show_lookback () =
    (match sim.waves with
     | Some waves ->
       Hardcaml_waveterm.Waveform.print
         waves
         ~wave_width:2
         ~display_width:120
         ~start_cycle:(sim.cycles - 16)
     | None -> print_endline "NOTE: consider turning on ~enable_waves for more debug info");
    Lookback.iter lookback ~f:(fun line -> print_endline (Trace_line.to_string line))
  in
  List.iter_until
    lines
    ~f:(fun cur ->
      let actual = Sim.trace sim in
      match [%equal: Trace_state.t] cur.state actual with
      | true ->
        Lookback.push lookback cur;
        (match Sim.step sim with
         | () -> Continue ()
         | exception exn ->
           show_lookback ();
           raise exn)
      | false ->
        Sim.cycle sim;
        show_lookback ();
        print_endline (Trace_line.to_string cur);
        print_endline (Trace_line.diff_line ~expected:cur ~actual);
        Stop ())
    ~finish:(fun () -> print_endline "all passed")
;;

let%expect_test "nestest trace comparison" =
  run_sim ~enable_waves:false;
  [%expect {| all passed |}]
;;

let%expect_test "State mapping" =
  Cpu.State.all
  |> List.iteri ~f:(fun n state ->
    printf "%02X: %s\n" n (Sexp.to_string [%sexp (state : Cpu.State.t)]));
  [%expect
    {|
    00: Reset
    01: Illegal
    02: Fetch
    03: Decode
    04: Perform_read
    05: Perform_write
    06: Perform_read_write1
    07: Perform_read_write2
    08: Absolute
    09: Zero_page_indexed
    0A: Absolute_indexed
    0B: Indexed_fix_cycle
    0C: Indirect_X1
    0D: Indirect_X2
    0E: Indirect_X3
    0F: Indirect_Y1
    10: Indirect_Y2
    11: Branch_taken
    12: Branch_fix_pch
    13: JMP
    14: JMPI1
    15: JMPI2
    16: JMPI3
    17: JSR1
    18: JSR2
    19: JSR3
    1A: JSR4
    1B: RTS1
    1C: RTS2
    1D: RTS3
    1E: RTS4
    1F: RTI1
    20: RTI2
    21: RTI3
    22: RTI4
    23: PHA
    24: PHP
    25: PLA1
    26: PLA2
    27: PLP1
    28: PLP2
    29: Interrupt0
    2A: Interrupt1
    2B: Interrupt2
    2C: Interrupt3
    2D: Interrupt4
    2E: Interrupt5
    |}]
;;
