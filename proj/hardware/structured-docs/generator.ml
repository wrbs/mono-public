open! Core
open! Async

module Index = struct
  type t =
    { category : int
    ; section : int option
    ; subsection : int option
    }
  [@@deriving compare]

  let of_string s =
    match String.split s ~on:'.' |> List.map ~f:Int.of_string with
    | [ a ] -> { category = a; section = None; subsection = None }
    | [ a; b ] -> { category = a; section = Some b; subsection = None }
    | [ a; b; c ] -> { category = a; section = Some b; subsection = Some c }
    | _ -> failwith "bad index"
  ;;

  let to_path t =
    let subpath =
      [ t.section; t.subsection ]
      |> List.filter_opt
      |> List.map ~f:Int.to_string
      |> String.concat ~sep:"_"
    in
    [%string "%{t.category#Int}/%{subpath}_"]
  ;;
end

let parse_index_md contents =
  let re =
    let open Re in
    let index =
      let parts n =
        let part = rep1 digit in
        let extra = List.create [ char '.'; part ] ~len:(n - 1) in
        group (seq (part :: List.concat extra))
      in
      group
        (alt
           [ seq [ parts 1; char '.' ]
           ; seq [ str "   * "; parts 2 ]
           ; seq [ str "       - "; parts 3 ]
           ])
    in
    Re.seq
      [ bol; index; str " ["; rep1 notnl; str "]("; group (rep1 notnl); str ")"; eol ]
  in
  let matches = Re.all (Re.compile re) contents in
  List.map matches ~f:(fun group ->
    Format.print_flush ();
    let index =
      List.init 3 ~f:(fun idx -> Re.Group.get_opt group (idx + 2))
      |> List.filter_opt
      |> List.hd_exn
      |> Index.of_string
    in
    let filename = Re.Group.get group 5 in
    ~index, ~filename)
;;

let command =
  Command.async ~summary:"spit out more navigable hardcaml docs"
  @@
  let%map_open.Command doc_dir = anon ("DOC_DIR" %: string)
  and output_dir = anon ("OUTPUT_DIR" %: string) in
  fun () ->
    let%bind index_md = Reader.file_contents (doc_dir ^/ "index.md") in
    let parsed = parse_index_md index_md in
    Deferred.List.iter parsed ~how:`Parallel ~f:(fun (~index, ~filename) ->
      let output_name = output_dir ^/ [%string "%{Index.to_path index}%{filename}"] in
      let%bind () = Unix.mkdir ~p:() (Filename.dirname output_name) in
      Unix.link ~target:(doc_dir ^/ filename) ~link_name:output_name ())
;;

let () = Command_unix.run command
