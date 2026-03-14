open! Core

let same_name_tools =
  [ "ocamlformat-rpc"
  ; "patdiff-git-wrapper"
  ; "sexp-group"
  ; "ocamllsp"
  ; "menhir"
  ; "ocp-indent"
  ; "ocp-indent-gen-rules"
  ; "ocamlformat"
  ; "patdiff"
  ; "sexp"
  ]
;;

let renamed_tools = [ "csv", "janestreet_csv" ]

module Repo_entry = struct
  type data = { full_repo : string } [@@sexp.allow_extra_fields] [@@deriving sexp]
  type t = string * data [@@deriving of_sexp]
end

let switch_template ~repo_arg =
  [%string
    {|#!/bin/bash

cd "$(dirname "$0")"

set -euo pipefail

if [[ -e "_opam" ]]; then
	echo "_opam present, refusing to proceed: mv to _opam.bak or delete _opam if last run failed"
	exit 1
fi

if [[ ! -e "_opam.bak" ]]; then
	echo "_opam.bak not present, refusing to proceed"
	exit
fi

opam switch create . 5.2.0+ox \
	--repos %{repo_arg} \
	--no-install
eval $(opam env --switch .)
opam install dune ocamlfind
|}]
;;

let make_switch =
  Command.basic ~summary:"generate the bash script"
  @@
  let%map_open.Command sexp_path = anon ("SEXP_PATH" %: string) in
  fun () ->
    let repos = Sexp.load_sexps_conv_exn sexp_path [%of_sexp: Repo_entry.t] in
    let repo_arg =
      [ "ox"; "opam" ]
      |> List.map ~f:(fun name ->
        List.find_map_exn repos ~f:(fun (repo, { full_repo }) ->
          let%map.Option () = Option.some_if ([%equal: string] repo name) () in
          let hash = String.rsplit2_exn full_repo ~on:'#' |> Tuple2.get2 in
          let short_hash = String.prefix hash 10 in
          [%string "%{name}-%{short_hash}=%{full_repo}"]))
      |> String.concat ~sep:","
    in
    print_string (switch_template ~repo_arg)
;;

let install_tools_template ~install_commands ~ensure_link_commands =
  [%string
    {|#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"
cd ..

eval $(opam env --switch . --set-switch)

set -x
dune build @tooling

mkdir -p _tools
%{install_commands}
set +x 

ensure_link() {
  local link="$1" target="$2"
  echo "linking $link -> $target"
  local abs_target
  abs_target="$(realpath "$target")"
  if [ ! -e "$link" ] && [ ! -L "$link" ]; then
    ln -s "$abs_target" "$link"
  elif [ -L "$link" ] && [ "$(readlink "$link")" = "$abs_target" ]; then
    return 0
  else
    echo "Error: '$link' exists but is not a symlink to '$abs_target'" >&2
    return 1
  fi
}

%{ensure_link_commands}
|}]
;;

let tools = renamed_tools @ List.map same_name_tools ~f:(fun n -> n, n)

let install_tools =
  Command.basic ~summary:"generate the bash script"
  @@
  let%map_open.Command () = return () in
  fun () ->
    let install_commands =
      List.map tools ~f:(fun (target_name, built_name) ->
        [%string
          "install -m 755 _build/install/default/bin/%{built_name} _tools/%{target_name}"])
      |> String.concat_lines
    in
    let ensure_link_commands =
      List.map tools ~f:(fun (bin, _) ->
        [%string "ensure_link _opam/bin/%{bin} _tools/%{bin}"])
      |> String.concat_lines
    in
    print_string (install_tools_template ~install_commands ~ensure_link_commands)
;;

let command =
  Command.group
    ~summary:"gen builder"
    [ "make-switch", make_switch; "install-tools", install_tools ]
;;

let () = Command_unix.run command
