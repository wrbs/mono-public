(ox (
  (full_repo
   git+https://github.com/oxcaml/opam-repository.git#afddc8337950dbc02863fe30ae58735a1cf2719f)
  (single_file_http
   https://raw.githubusercontent.com/oxcaml/opam-repository/afddc8337950dbc02863fe30ae58735a1cf2719f/)))

(dune-overlays (
  (full_repo
   git+https://github.com/dune-universe/opam-overlays.git#632df22c65362cb34fd792f701d7d2a6518cdb60)
  (single_file_http
   https://raw.githubusercontent.com/dune-universe/opam-overlays/632df22c65362cb34fd792f701d7d2a6518cdb60/)
  (filter (
    Include (
      cmdliner
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
      uunf
      uutf
      xmlm)))))

(opam (
  (full_repo
   git+https://github.com/ocaml/opam-repository.git#2ffc3518756432a5bea7cda8ae467ea8b4e0362a)
  (single_file_http
   https://raw.githubusercontent.com/ocaml/opam-repository/2ffc3518756432a5bea7cda8ae467ea8b4e0362a/)))
