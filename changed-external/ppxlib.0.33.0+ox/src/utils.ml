open Import

(* We want to make sure we only convert actual odoc comments [(** ... *)] and not
   actual instances of [@@ocaml.doc "..."]. When parsed, both get translated as
   an attribute.

   To differentiate them, we can take advantage of the fact that the location
   attached to the attribute node for (** ... *) comments is equal to the location of
   the string itself, while for [@@ocaml.doc "..."] they are different.

   The same is true for [@@@ocaml.text]. *)
let get_odoc_contents_if_comment = function
  | {
      attr_loc;
      attr_name = { txt = ("doc" | "ocaml.doc" | "text" | "ocaml.text"); _};
      attr_payload =
        PStr [
          {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_constant (Pconst_string (text, loc, _)); _ }, _);
            _
          }
        ];
    } when Location.compare attr_loc loc = 0 -> Some text
  | _ -> None

let prettify_odoc_attributes =
  object
    inherit Ast_traverse.map as super

    method! attribute attr =
      let attr = super#attribute attr in
      match get_odoc_contents_if_comment attr with
      | Some txt ->
        let open Ast_builder.Default in
        let loc = Location.none in
        let delim = Some (Common.valid_string_constant_delimiter txt) in
        let expr = pexp_constant ~loc (Pconst_string (txt, loc, delim)) in
        { attr with attr_payload = PStr [ pstr_eval ~loc expr [] ] }
      | None -> attr
  end

let merge_locs loc1 loc2 ~loc_ghost =
  let loc_start = Location.min_pos loc1.loc_start loc2.loc_start in
  let loc_end = Location.max_pos loc1.loc_end loc2.loc_end in
  { loc_start; loc_end; loc_ghost }

let get_doc_comments_loc_bounds =
  object
    inherit [location option] Ast_traverse.fold as super

    method! attribute attr acc =
      let acc = super#attribute attr acc in
      match acc with
      | Some loc when Option.is_some (get_odoc_contents_if_comment attr) ->
        Some (merge_locs loc attr.attr_loc ~loc_ghost:true)
      | _ -> Some attr.attr_loc
  end


let update_locs_to_include_doc_comments =
  object
    inherit Ast_traverse.map as super

    method! structure_item item =
      let item = super#structure_item item in
      match get_doc_comments_loc_bounds#structure_item item None with
      | Some loc ->
        { item with
          pstr_loc =
            merge_locs
              ~loc_ghost:item.pstr_loc.loc_ghost
              item.pstr_loc
              loc
        }
      | None -> item

    method! signature_item item =
      let item = super#signature_item item in
      match get_doc_comments_loc_bounds#signature_item item None with
      | Some loc ->
        { item with
          psig_loc =
            merge_locs
              ~loc_ghost:item.psig_loc.loc_ghost
              item.psig_loc
              loc
        }
      | None -> item
  end

let with_output fn ~binary ~f =
  match fn with
  | None | Some "-" -> f stdout
  | Some fn -> Out_channel.with_file fn ~binary ~f

module Kind = struct
  type t = Intf | Impl

  let of_filename fn : t option =
    if Stdlib.Filename.check_suffix fn ".ml" then Some Impl
    else if Stdlib.Filename.check_suffix fn ".mli" then Some Intf
    else None

  let describe = function Impl -> "implementation" | Intf -> "interface"
  let equal : t -> t -> bool = Poly.equal
end

module Intf_or_impl = struct
  type t = Intf of signature | Impl of structure

  let map t (map : Ast_traverse.map) =
    match t with
    | Impl x -> Impl (map#structure x)
    | Intf x -> Intf (map#signature x)

  let map_with_context t (map : _ Ast_traverse.map_with_context) ctx =
    match t with
    | Impl x -> Impl (map#structure ctx x)
    | Intf x -> Intf (map#signature ctx x)

  let kind : _ -> Kind.t = function Intf _ -> Intf | Impl _ -> Impl
end

module Ast_io = struct
  type input_version = (module OCaml_version)

  let fall_back_input_version = (module Compiler_version : OCaml_version)
  (* This should only be used when the input version can't be determined due to
      loading or preprocessing errors *)

  type t = {
    input_name : string;
    input_version : input_version;
    ast : Intf_or_impl.t;
  }

  type read_error =
    | Not_a_binary_ast
    | Unknown_version of string * input_version
    | Source_parse_error of Location.Error.t * input_version
    | System_error of Location.Error.t * input_version

  type input_source = Stdin | File of string
  type input_kind = Possibly_source of Kind.t * string | Necessarily_binary

  let read_error_to_string (error : read_error) =
    match error with
    | Not_a_binary_ast -> "Error: Not a binary ast"
    | Unknown_version (s, _) -> "Error: Unknown version " ^ s
    | Source_parse_error (loc, _) ->
        "Source parse error:" ^ Location.Error.message loc
    | System_error (loc, _) -> "System error: " ^ Location.Error.message loc

  let parse_source_code ~(kind : Kind.t) ~input_name ~prefix_read_from_source ic
      =
    (* The input version is determined by the fact that the input will get parsed by
       the current compiler Parse module *)
    let input_version = (module Compiler_version : OCaml_version) in
    try
      (* To test if a file is an AST file, we have to read the first few bytes of the
          file. If it is not, we have to parse these bytes and the rest of the file as
          source code.

          The compiler just does [seek_on 0] in this case, however this doesn't work when
          the input is a pipe.

          What we do instead is create a lexing buffer from the input channel and pre-fill
          it with what we read to do the test. *)
      let lexbuf = Lexing.from_channel ic in
      let len = String.length prefix_read_from_source in
      Bytes.blit_string ~src:prefix_read_from_source ~src_pos:0
        ~dst:lexbuf.lex_buffer ~dst_pos:0 ~len;
      lexbuf.lex_buffer_len <- len;
      lexbuf.lex_curr_p <-
        { pos_fname = input_name; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      Skip_hash_bang.skip_hash_bang lexbuf;
      (* Force-enable all Jane Street language extensions, which enables standalone
         ppx drivers to parse all constructs. *)
      Ocaml_common.Language_extension.(set_universe_and_enable_all Universe.maximal);
      let ast : Intf_or_impl.t =
        match kind with
        | Intf -> Intf (Parse.interface lexbuf)
        | Impl -> Impl (Parse.implementation lexbuf)
      in
      Ok { input_name; input_version; ast }
    with exn -> (
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error -> Error (Source_parse_error (error, input_version)))

  let magic_length = String.length Astlib.Config.ast_impl_magic_number

  let read_magic ic =
    let buf = Bytes.create magic_length in
    let len = input ic buf 0 magic_length in
    let s = Bytes.sub_string buf ~pos:0 ~len in
    if len = magic_length then Ok s else Error s

  let from_channel ch ~input_kind =
    let handle_non_binary prefix_read_from_source =
      match input_kind with
      | Possibly_source (kind, input_name) ->
          parse_source_code ~kind ~input_name ~prefix_read_from_source ch
      | Necessarily_binary -> Error Not_a_binary_ast
    in
    match read_magic ch with
    | Error s -> handle_non_binary s
    | Ok s -> (
        match Find_version.from_magic s with
        | Intf (module Input_version : OCaml_version) ->
            let input_name : string = input_value ch in
            let ast = input_value ch in
            let module Input_to_ppxlib = Convert (Input_version) (Js) in
            let ast = Intf_or_impl.Intf (Input_to_ppxlib.copy_signature ast) in
            Ok
              {
                input_name;
                input_version = (module Input_version : OCaml_version);
                ast;
              }
        | Impl (module Input_version : OCaml_version) ->
            let input_name : string = input_value ch in
            let ast = input_value ch in
            let module Input_to_ppxlib = Convert (Input_version) (Js) in
            let ast = Intf_or_impl.Impl (Input_to_ppxlib.copy_structure ast) in
            Ok
              {
                input_name;
                input_version = (module Input_version : OCaml_version);
                ast;
              }
        | Unknown ->
            if
              String.equal
                (String.sub s ~pos:0 ~len:9)
                (String.sub Astlib.Config.ast_impl_magic_number ~pos:0 ~len:9)
              || String.equal
                   (String.sub s ~pos:0 ~len:9)
                   (String.sub Astlib.Config.ast_intf_magic_number ~pos:0 ~len:9)
            then Error (Unknown_version (s, fall_back_input_version))
            else handle_non_binary s)

  let read input_source ~input_kind =
    try
      match input_source with
      | Stdin -> from_channel stdin ~input_kind
      | File fn -> In_channel.with_file fn ~f:(from_channel ~input_kind)
    with exn -> (
      match Location.Error.of_exn exn with
      | None -> raise exn
      | Some error -> Error (System_error (error, fall_back_input_version)))

  let write oc { input_name; input_version = (module Input_version); ast }
      ~add_ppx_context =
    let module Ppxlib_to_input = Convert (Js) (Input_version) in
    let module Ocaml_to_input = Convert (Compiler_version) (Input_version) in
    match ast with
    | Intf sg ->
        let sg =
          if add_ppx_context then
            Selected_ast.To_ocaml.copy_signature sg
            |> Astlib.Ast_metadata.add_ppx_context_sig ~tool_name:"ppx_driver"
            |> Ocaml_to_input.copy_signature
          else Ppxlib_to_input.copy_signature sg
        in
        output_string oc Input_version.Ast.Config.ast_intf_magic_number;
        output_value oc input_name;
        output_value oc sg
    | Impl st ->
        let st =
          if add_ppx_context then
            Selected_ast.To_ocaml.copy_structure st
            |> Astlib.Ast_metadata.add_ppx_context_str ~tool_name:"ppx_driver"
            |> Ocaml_to_input.copy_structure
          else Ppxlib_to_input.copy_structure st
        in
        output_string oc Input_version.Ast.Config.ast_impl_magic_number;
        output_value oc input_name;
        output_value oc st

  module Read_bin = struct
    type ast = Intf of signature | Impl of structure
    type t = { ast : ast; input_name : string }

    let read_binary fn =
      match
        In_channel.with_file fn ~f:(from_channel ~input_kind:Necessarily_binary)
      with
      | Ok { ast; input_name; _ } ->
          let ast =
            match ast with
            | Impl structure -> Impl structure
            | Intf signature -> Intf signature
          in
          Ok { ast; input_name }
      | Error e -> Error (read_error_to_string e)

    let get_ast t = t.ast
    let get_input_name t = t.input_name
  end
end

module System = struct
  let run_preprocessor ~pp ~input ~output =
    let command =
      Printf.sprintf "%s %s > %s" pp
        (if String.equal input "-" then "" else Stdlib.Filename.quote input)
        (Stdlib.Filename.quote output)
    in
    if Stdlib.Sys.command command = 0 then Ok ()
    else Error (command, Ast_io.fall_back_input_version)
end
