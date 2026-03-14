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

opam switch create . 5.2.0+ox \
	--repos ox-dde24fbc07=git+https://github.com/oxcaml/opam-repository.git#dde24fbc07390ada8c41508871d4741e06069241,opam-8d34e0cf3c=git+https://github.com/ocaml/opam-repository.git#8d34e0cf3c0ccacb6c8a26c24d0e5eb0b17fbf9d \
	--no-install
eval $(opam env --switch .)
opam install dune
