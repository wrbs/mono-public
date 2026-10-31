The new 5.5 syntax for module dependent functions should be correctly
indented:

  $ cat > test.ml << EOF
  > let
  > sort
  > (
  > module
  > MSet
  > :
  > Set.S
  > )
  > l
  > =
  > MSet.elements (MSet.of_list l)
  > EOF

  $ ocp-indent test.ml
  let
    sort
      (
        module
          MSet
          :
            Set.S
      )
      l
    =
    MSet.elements (MSet.of_list l)
