open! Core

module Mirroring = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving sexp_of]
end

type t =
  { mapper : int
  ; mirroring : Mirroring.t
  ; alternate_nametable : bool
  ; provide_prg_ram : bool
  ; prg_rom : Bigstring.Hexdump.t
  ; chr_rom : Bigstring.Hexdump.t option
  }
[@@deriving sexp_of]

let check_magic iobuf =
  let magic = Iobuf.Peek.stringo iobuf ~pos:0 ~len:4 in
  let expected = "NES\x1a" in
  if not ([%equal: string] magic expected)
  then raise_s [%message "Invalid magic bytes" (magic : string) (expected : string)]
;;

let flag_bit flags bit = (flags lsr bit) land 1 = 1

let parse_exn iobuf =
  check_magic iobuf;
  let header = Iarray.init 16 ~f:(fun _ -> Iobuf.Consume.uint8 iobuf) in
  let prg_rom_size = header.:(4) * 16384 in
  let chr_rom_size = header.:(5) * 8192 in
  let mirroring : Mirroring.t =
    match flag_bit header.:(6) 0 with
    | false -> Horizontal
    | true -> Vertical
  in
  let provide_prg_ram = flag_bit header.:(6) 1 in
  let provide_trainer = flag_bit header.:(6) 2 in
  if provide_trainer then failwith "Trainers not supported";
  let alternate_nametable = flag_bit header.:(6) 3 in
  let mapper = header.:(7) land 0xF0 lor (header.:(6) lsr 4) in
  let prg_rom = Iobuf.Consume.bigstringo iobuf ~len:prg_rom_size in
  let chr_rom =
    Option.some_if_thunk (chr_rom_size > 0) (fun () ->
      Iobuf.Consume.bigstringo iobuf ~len:chr_rom_size)
  in
  { mapper; mirroring; alternate_nametable; provide_prg_ram; prg_rom; chr_rom }
;;

let of_iobuf iobuf = Or_error.try_with @@ fun () -> parse_exn iobuf
let of_bigstring s = s |> Iobuf.of_bigstring |> of_iobuf
let of_string s = s |> Iobuf.of_string |> of_iobuf
