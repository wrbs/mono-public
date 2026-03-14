(ox (
  (full_repo
   git+https://github.com/oxcaml/opam-repository.git#dde24fbc07390ada8c41508871d4741e06069241)
  (single_file_http
   https://raw.githubusercontent.com/oxcaml/opam-repository/dde24fbc07390ada8c41508871d4741e06069241/)))

(dune-overlays (
  (full_repo
   git+https://github.com/dune-universe/opam-overlays.git#12731a6f86d7c452a94c72106fa9d3327988582d)
  (single_file_http
   https://raw.githubusercontent.com/dune-universe/opam-overlays/12731a6f86d7c452a94c72106fa9d3327988582d/)
  (filter (
    Include (
      astring
      cmarkit
      findlib
      fmt
      fpath
      jsonm
      logs
      num
      ocamlfind
      seq
      uchar
      uucp
      uutf
      xmlm)))))

(opam (
  (full_repo
   git+https://github.com/ocaml/opam-repository.git#8d34e0cf3c0ccacb6c8a26c24d0e5eb0b17fbf9d)
  (single_file_http
   https://raw.githubusercontent.com/ocaml/opam-repository/8d34e0cf3c0ccacb6c8a26c24d0e5eb0b17fbf9d/)))
