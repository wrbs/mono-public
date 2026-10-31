# Contributing to `ocp-indent`

## Setting up your dev environment

You can quickly setup your `ocp-indent` dev environement by running:
```
git clone git@github.com:OCamlPro/ocp-indent.git
cd ocp-indent
opam switch create ./ --deps-only -t
```

This will clone the project and create a local opam switch with all of the
project's dependencies required to build it and run the tests.

## Building `ocp-indent`

The project uses dune so you can build it using the usual:
```
dune build
```

## Project layout

All our source code leaves in `src/`:
- `ocp-indent-lib` contains the core ocp-indent logic
- `ocp-indent-lexer` contains our simplified OCaml lexer
- `ocp-indent` is the CLI wrapper around the lib and parser
- `ocp-indent-utils` contains small helpers and stdlib extensions
- `ocp-indent-dynlink` is the plugin loading lib for `--load-pkgs` and
  `--load-mods`
- `ocp-indent-gen-rules` is a small executable used to generate dune formatting
  rules with `ocp-indent`

The two most important modules are `IndentBlock` and `IndentPrinter`.

`IndentPrinter` runs through the lexer token stream and calculates the
indentation for each new line that appears. This is done by asking `indentBlock`
to calculate a structured "path" to the current token. This path reflect the
OCaml structure of the code and is used to calculate the indentation level of
the current token if it sits at the begining of a line.

Most changes usually go in either of those modules which makes for the core of
the tool's logic.

## Tests

You can run the full test suite with:
```
dune runtest
```

All of our tests are dune cram tests (you can learn more
[here](https://dune.readthedocs.io/en/stable/reference/cram.html)).

The main executable tests are separated between three folders:
- `tests/passing`: for regular tests, running on all platforms
- `tests/unix-only`: for tests that cannot be run on Windows
- `tests/failing`: for tests that document known bugs. Those have their own
  [README](tests/failing/README.md) if you ever need to know more.

`ocp-indent-gen-rules` has its own test folder in `tests/ocp-indent-gen-rules`.

## Formatting

The code is indented using, well, ocp-indent. Dune files are formatted as well.

Formatting is checked by our CI so remember to run:

```
dune build @fmt
```

to ensure your patches are correctly formatted. If the command reports a diff,
you can run `dune promote` to update the relevant files.

## Submitting contributions

We expect the usual github workflow for external contributions.

Feel free to open a PR directly for simple bug fixes, typos and other consensual
changes.

When submitting a bug fix, please try to add a regression tests alongside it if
it's reasonably easy to write.

For more involved changes/features, we suggest that you start by opening an
issue so you can discuss with the maintainers team the best way to proceed.
