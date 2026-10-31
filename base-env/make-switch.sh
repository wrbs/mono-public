#!/bin/bash

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

opam switch create . \
	--packages oxcaml-compiler.5.2.0minus40 \
	--repos ox-afddc83379=git+https://github.com/oxcaml/opam-repository.git#afddc8337950dbc02863fe30ae58735a1cf2719f,opam-2ffc351875=git+https://github.com/ocaml/opam-repository.git#2ffc3518756432a5bea7cda8ae467ea8b4e0362a \
	--no-install
eval $(opam env --switch .)

# optimized dune
OCAMLPARAM="_,O3=1" opam install dune
opam install ocamlfind
