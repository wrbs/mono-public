Strict mode makes all warnings fatal, meaning execution stops on any warning
and exits with non zero status

  $ cat > test.ml << EOF
  > let x = 0
  > EOF

  $ ocp-indent --syntax foo test.ml
  ocp-indent warning: unknown syntax extension "foo"
  let x = 0

  $ ocp-indent --strict --syntax foo test.ml
  ocp-indent error: unknown syntax extension "foo"
  [123]

---------------------------------------------------------

  $ cat > .ocp-indent << EOF
  > unknown_key=foo
  > EOF

  $ ocp-indent test.ml 2>&1 | sed -e 's/".*ocp-indent"/<dot-ocp-indent>/' -
  ocp-indent warning: error in configuration file <dot-ocp-indent>:
  unknown configuration key "unknown_key"
  let x = 0

  $ ocp-indent --strict test.ml 2>&1 | sed -e 's/".*ocp-indent"/<dot-ocp-indent>/' -
  ocp-indent error: error in configuration file <dot-ocp-indent>:
  unknown configuration key "unknown_key"

  $ rm .ocp-indent

---------------------------------------------------------

  $ OCP_INDENT_CONFIG=foo ocp-indent test.ml
  ocp-indent warning: invalid $OCP_INDENT_CONFIG
  let x = 0

  $ OCP_INDENT_CONFIG=foo ocp-indent --strict test.ml
  ocp-indent error: invalid $OCP_INDENT_CONFIG
  [123]

---------------------------------------------------------

  $ printf 'let x =\n\t0\n' > test.ml

  $ ocp-indent -l 1 test.ml
  ocp-indent warning: input contains indentation by tabs, partial indent will be unreliable.
  let x =
  	0

  $ ocp-indent --strict -l 1 test.ml
  ocp-indent error: input contains indentation by tabs, partial indent will be unreliable.
  let x =
  [123]

---------------------------------------------------------

Strict mode also makes the parsing a bit stricter, rejecting some obvious syntax
errors:

  $ cat > test.ml << EOF
  > let x = f ~if:0
  > EOF

  $ ocp-indent test.ml
  let x = f ~if:0

  $ ocp-indent --strict test.ml
  ocp-indent parsing error: File test.ml, line 1, 10-14:
  Invalid tuple or argument label: if
  let x = f
  [123]

---------------------------------------------------------

  $ cat > test.ml << EOF
  > let x = "\e"
  > EOF

  $ ocp-indent test.ml
  let x = "\e"

  $ ocp-indent --strict test.ml
  ocp-indent parsing error: File test.ml, line 1, 9-11:
  Illegal character escape: \e
  let x
  [123]
