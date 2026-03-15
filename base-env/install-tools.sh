#!/bin/bash

set -euo pipefail

cd "$(dirname "$0")"
cd ..

eval $(opam env --switch . --set-switch)

context="${CONTEXT:-opt}"

set -x
dune build @_build/$context/tooling

mkdir -p _tools
install -m 755 _build/install/${context}/bin/janestreet_csv _tools/csv
install -m 755 _build/install/${context}/bin/ocamlformat-rpc _tools/ocamlformat-rpc
install -m 755 _build/install/${context}/bin/patdiff-git-wrapper _tools/patdiff-git-wrapper
install -m 755 _build/install/${context}/bin/sexp-group _tools/sexp-group
install -m 755 _build/install/${context}/bin/ocamllsp _tools/ocamllsp
install -m 755 _build/install/${context}/bin/menhir _tools/menhir
install -m 755 _build/install/${context}/bin/ocp-indent _tools/ocp-indent
install -m 755 _build/install/${context}/bin/ocp-indent-gen-rules _tools/ocp-indent-gen-rules
install -m 755 _build/install/${context}/bin/ocamlformat _tools/ocamlformat
install -m 755 _build/install/${context}/bin/patdiff _tools/patdiff
install -m 755 _build/install/${context}/bin/sexp _tools/sexp

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

ensure_link _opam/bin/csv _tools/csv
ensure_link _opam/bin/ocamlformat-rpc _tools/ocamlformat-rpc
ensure_link _opam/bin/patdiff-git-wrapper _tools/patdiff-git-wrapper
ensure_link _opam/bin/sexp-group _tools/sexp-group
ensure_link _opam/bin/ocamllsp _tools/ocamllsp
ensure_link _opam/bin/menhir _tools/menhir
ensure_link _opam/bin/ocp-indent _tools/ocp-indent
ensure_link _opam/bin/ocp-indent-gen-rules _tools/ocp-indent-gen-rules
ensure_link _opam/bin/ocamlformat _tools/ocamlformat
ensure_link _opam/bin/patdiff _tools/patdiff
ensure_link _opam/bin/sexp _tools/sexp

