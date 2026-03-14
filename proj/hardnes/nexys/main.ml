open! Core
open! Hardcaml

open struct
  open Hardcaml_hobby_boards
  module Board = Board
  module Nexys = Nexys_a7_100t
end

module Cur_toplevel = Ethernet_test

let generate_top =
  Command.basic ~summary:"generate the toplevel in the directory"
  @@
  let%map_open.Command dir =
    flag [%var_dash_name] (required string) ~doc:"- path to dir to generate"
  in
  fun () ->
    let board = Cur_toplevel.create () in
    Nexys.generate_top board ~dir
;;

let change_reset_vector (rom : Nes_file.t) =
  let len = Bigstring.length rom.prg_rom in
  let set a n = Bigstring.set rom.prg_rom (len - (0x10000 - a)) (Char.of_int_exn n) in
  set 0xFFFC 0x00;
  set 0xFFFD 0xC0
;;

let dump_rom =
  Command.basic ~summary:"nestest dump"
  @@
  let%map_open.Command input = anon ("INPUT" %: string) in
  fun () ->
    let contents = In_channel.read_all input in
    let rom = Nes_file.of_string contents |> Or_error.ok_exn in
    change_reset_vector rom;
    print_string "@0000";
    for i = 0 to Bigstring.length rom.prg_rom - 1 do
      print_string (if i % 8 = 0 then "\n" else " ");
      printf "%02X" (Char.to_int (Bigstring.get rom.prg_rom i))
    done
;;

let build_files =
  Command.basic ~summary:"save the toplevel v and xdc"
  @@
  let%map_open.Command name =
    flag "name" (required string) ~doc:"_ name of the toplevel part"
  and dir = flag "dir" (required string) ~doc:"_ dir to write files in" in
  fun () ->
    let board = Cur_toplevel.create () in
    let ~verilog, ~xdc = Nexys.generate_files board ~name in
    let filename ext = dir ^/ [%string "%{name}.%{ext}"] in
    Out_channel.write_all (filename "v") ~data:verilog;
    Out_channel.write_all (filename "xdc") ~data:xdc
;;

let command =
  Command.group
    ~summary:"Hardnes commands"
    [ "generate-top", generate_top; "build-files", build_files; "dump-rom", dump_rom ]
;;

let run () = Command_unix.run command
