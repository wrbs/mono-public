# mono
A (subset of) my o(x)caml monorepo. Only includes certain projects/libraries I want
to share publically.

Has vendored dependencies in `vendor` made using
[oxcaml-vendor-tool](https://github.com/wrbs/oxcaml-vendor-tool/).

## Install instructions

To try and give maximum flexibility for patching, only the base compiler, `dune`
and `ocamlfind` are installed into the switch -- everything else is vendored
into dune.

Because it gave me issues in the past, the workflow now specifically pins a
version of oxcaml & the default opam repository.

Dev-tools are built locally in the repo and copied/linked into the switch by 
`base-env/install-tools.sh`.

### First install

    touch base-env/_opam.bak
    base-env/make-switch.sh
    opam switch link ./base-env
    eval $(opam env)
    base-env/install-tools.sh
    # OR to get less optimized tools
    # CONTEXT=default base-env/install-tools.sh

### Upgrading

    mv base-env/_opam base-env/_opam.bak
    base-env/make-switch.sh
    rm -rf _build
    base-env/install-tools.sh
    # OR to get less optimized tools
    # CONTEXT=default base-env/install-tools.sh

## Using

Build everything with dune _in the default context_ (or else you'll build
optimized at the same time)

    dune build --watch @_build/default/default @_build/default/runtest

For opt builds (I'd avoid tbh outside of tools)

    dune build --watch @_build/opt/default @_build/opt/runtest