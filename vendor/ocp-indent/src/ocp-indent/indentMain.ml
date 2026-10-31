(**************************************************************************)
(*                                                                        *)
(*  Copyright 2011 Jun Furuse                                             *)
(*  Copyright 2012,2013 OCamlPro                                          *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 2.1 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  TypeRex is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  Lesser GNU General Public License for more details.                   *)
(*                                                                        *)
(**************************************************************************)

module Args = IndentArgs

let indent_channel ~filename ic args config out perm =
  let oc, need_close = match out with
    | None | Some "-" -> stdout, false
    | Some file -> open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] perm file, true
  in
  let output = {
    IndentPrinter.
    debug = args.Args.debug;
    config = config;
    in_lines = args.Args.in_lines;
    indent_empty = args.Args.indent_empty;
    adaptive = true;
    kind = args.Args.indent_printer oc;
  }
  in
  let stream = Nstream.of_channel ~filename ic in
  IndentPrinter.proceed output stream IndentBlock.empty ();
  flush oc;
  if need_close then close_out oc

let check_channel ~filename ic (args : Args.t) config =
  let output : unit IndentPrinter.output =
    { debug = args.debug
    ; config
    ; in_lines = args.in_lines
    ; indent_empty = args.indent_empty
    ; adaptive = true
    ; kind = Print (fun _ () -> ())
    }
  in
  let stream = Nstream.of_channel ~filename ic in
  let ret = IndentPrinter.check output stream IndentBlock.empty in
  if not ret then
    Printf.eprintf "%s\n" filename;
  ret

let config_syntaxes syntaxes =
  Approx_lexer.disable_extensions ();
  List.iter (fun stx ->
      try
        Approx_lexer.enable_extension stx
      with IndentExtend.Syntax_not_found name ->
        IndentWarning.emit "unknown syntax extension %S" name)
    syntaxes

let indent_file args = function
  | Args.InChannel ic ->
      let filename = "<stdin>" in
      let config, syntaxes, dlink = IndentConfig.local_default () in
      IndentLoader.load ~debug:args.Args.debug (dlink @ args.Args.dynlink);
      config_syntaxes (syntaxes @ args.Args.syntax_exts);
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      if args.Args.check then
        check_channel ~filename ic args config
      else begin
        indent_channel ~filename ic args config args.Args.file_out 0o644;
        true
      end
  | Args.File path ->
      let config, syntaxes, dlink =
        IndentConfig.local_default ~path:(Filename.dirname path) ()
      in
      IndentLoader.load ~debug:args.Args.debug (dlink @ args.Args.dynlink);
      config_syntaxes (syntaxes @ args.Args.syntax_exts);
      let config =
        List.fold_left
          IndentConfig.update_from_string
          config
          args.Args.indent_config
      in
      let ic = open_in_bin path in
      if args.Args.check then
        check_channel ~filename:path ic args config
      else
        let out, perm, need_move =
          if args.Args.inplace then
            let tmp_file = path ^ ".ocp-indent-tmp" in
            let rec get_true_file path =
              let open Unix in
              match lstat path with
              | { st_kind = S_REG ; st_perm } -> Some tmp_file, st_perm, Some path
              | { st_kind = S_LNK ; } -> get_true_file @@ readlink path
              | { st_kind = _ ; } -> failwith "invalid file type"
            in get_true_file path
          else
            args.Args.file_out, 0o644, None
        in
        try
          indent_channel ~filename:path ic args config out perm;
          (match out, need_move with
           | Some src, Some dst -> Sys.rename src dst
           | _, _ -> ());
          true
        with e ->
          close_in ic; raise e

let handle_errors f =
  try f () with
  | IndentWarning.Fatal msg ->
      Printf.eprintf "ocp-indent error: %s\n%!" msg;
      exit Cmdliner.Cmd.Exit.some_error
  | Approx_lexer.Error {error; start_pos; end_pos} ->
      let msg = Approx_lexer.error_msg error in
      (if Int.equal start_pos.pos_lnum end_pos.pos_lnum then
         let start_char, end_char =
           start_pos.pos_cnum - start_pos.pos_bol,
           end_pos.pos_cnum - end_pos.pos_bol
         in
         Format.eprintf
           "ocp-indent parsing error:@ \
            File %s, line %d, %d-%d:@ %s\n%!"
           start_pos.pos_fname start_pos.pos_lnum start_char end_char msg
       else
         Format.eprintf
           "ocp-indent parsing error:@ \
            File %s, line %d-%d:@ %s\n%!"
           start_pos.pos_fname start_pos.pos_lnum end_pos.pos_lnum msg);
      exit Cmdliner.Cmd.Exit.some_error
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      Printexc.raise_with_backtrace e bt

let main =
  Cmdliner.Cmd.v Args.info
    Cmdliner.Term.(
      const
        (fun (args,files) ->
           IndentWarning.set_strict_mode args.Args.strict;
           Approx_lexer.set_strict_mode args.Args.strict;
           let successes =
             List.map
               (fun file -> handle_errors (fun () -> indent_file args file))
               files
           in
           if List.for_all (fun x -> x) successes then
             Cmdliner.Cmd.Exit.ok
           else
             Cmdliner.Cmd.Exit.some_error
        )
      $ Args.options
    )


let () =
  exit (Cmdliner.Cmd.eval' main)
