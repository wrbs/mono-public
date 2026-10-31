Check mode does not actually indent anything but rather checks
that the given file(s) are correctly indented. If they are not, the
filename is printed on stderr and we exit with non zero code:

  $ cat > good.ml << EOF
  > let x =
  >   0
  > EOF

  $ cat > bad.ml << EOF
  > let x =
  > 0
  > EOF

  $ ocp-indent --check good.ml

  $ ocp-indent --check bad.ml
  bad.ml
  [123]

The --check option is incompatible with any output related option:

  $ ocp-indent --check good.ml -o output.ml
  ocp-indent: incompatible options used with --check
  [124]

  $ ocp-indent --check good.ml --numeric
  ocp-indent: incompatible options used with --check
  [124]

  $ ocp-indent --check good.ml --inplace
  ocp-indent: incompatible options used with --check
  [124]

When given multiple files, all files are checked, we don't stop at the first
misindented file. We exit with zero only if all files are correctly indented:

  $ cat > good2.ml << EOF
  > let x = 1
  > EOF

  $ cat > bad2.ml << EOF
  >      let x = 1
  > EOF

  $ ocp-indent --check bad.ml bad2.ml
  bad.ml
  bad2.ml
  [123]

  $ ocp-indent --check good.ml good2.ml

  $ ocp-indent --check bad.ml good.ml
  bad.ml
  [123]

Check mode works correctly with --lines, i.e. it fails only if lines in the
given range are misindented :

  $ cat > range.ml << EOF
  > let x = (* line 1 *)
  > 2 (* line 2, misindented compared to 1 *)
  > + 3 (* line 3, misindented compared to 1 but not 2 *)
  > + 4
  >                + 5
  >                + 6
  > + 7
  > EOF

  $ ocp-indent --check range.ml --lines 3-4

  $ ocp-indent --check range.ml --lines 5-6
  range.ml
  [123]

Check mode should not omit to check for multiline tokens (different code
path from regular indentation):

  $ cat > multiline.ml << EOF
  > let x =
  >   "some string spanning on \\
  >         multiple lines, being poorly indented"
  > EOF

  $ ocp-indent --check multiline.ml
  multiline.ml
  [123]
